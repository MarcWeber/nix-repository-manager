{-# LANGUAGE ScopedTypeVariables #-}
module Implementations where
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as BS
import Codec.Archive.Tar.Entry
import Data.Maybe
import qualified Data.Map as M
import Data.List
import System.Directory
import System.Posix.Files
import System.Environment
import System.IO
import System.FilePath.Glob
import System.FilePath
import Control.Monad
import Types
import Repo
import Util
import NixFile
import Codec.Compression.BZip (compress)


autoUpdateImpl, hackNixImpl, gemImpl ::
        String      -- tmpDir
        -> String   -- revId
        -> Config
        -> IRegionData 
      -> IO [String] -- region contents without indentation

source :: Config -> FilePath -> String -> IO String
source cfg file snapshotName = addErrorContext "source" $ do
    hash <- createHash file Sha256 (file ++ ".sha256")
    return $ "(fetchurl { url = \"" ++ cfgUrl cfg ++ snapshotName ++ "\"; sha256 = \"" ++ hash ++ "\"; })"

createTarBz2 :: Config -> IRegionData -> String -> IO String -- basename of snapshot file
createTarBz2 cfg reg rev = addErrorContext "createTarBz2" $ do
  let name = regionName reg
  let r = fromJust $ repoFromMap (rOpts reg)
  let snapshotName = name ++ nameSuffix r rev ++ ".tar.bz2"
  let destFile = (cfgRepoDir cfg) </> "dist" </> snapshotName
  let (repoDir :: String) = (cfgRepoDir cfg) </> name
  sf <- sourceFiles r repoDir

  entries <- mapM (\p -> do
    let x = "nix_repository_manager" </> p
    let s = repoDir </> p
    isd <- doesDirectoryExist s
    packFileEntry s (either error id (toTarPath isd x))
    ) sf
  BS.writeFile destFile $ compress $ Tar.write entries
  return snapshotName

-- auto update implementation {{{1
-- source only - no fancy stuff
autoUpdateImpl _ rev cfg reg = addErrorContext "autoUpdateImpl" $ do
    let name = regionName reg
    let repoDir = (cfgRepoDir cfg) -- copied some lines below 
    let distDir = repoDir </> "dist"
    let snapshotFileCache = distDir </> name
    let fields = words $ M.findWithDefault "src name" "fields" (rOpts reg)
    let r = fromJust $ repoFromMap (rOpts reg)
    let suffix = nameSuffix r rev

    snapshotName <- createTarBz2 cfg reg rev
    writeFile snapshotFileCache snapshotName

    src <- source cfg (distDir </> snapshotName) snapshotName

    let fieldToStr "src"     = "src = " ++ src ++ ";"
        fieldToStr "srcFile" = "srcFile = " ++ src ++ ";"
        fieldToStr "name" = "name = \"" ++ name ++ suffix ++ "\";"
        fieldToStr _ = error "autoUpdateImpl: unexpected field"

    -- no fieldds forced only return source without attr name
    return $ if null fields then [src]
                   else map fieldToStr fields

-- hacknix implementation {{{1
-- runs also hack-nix to get dependency information from .cabal file
hackNixImpl tmpDir rev cfg reg = addErrorContext "hackNixImpl" $ do
    let repoDir = cfgRepoDir cfg
    let name = regionName reg
    let distDir = repoDir </> "dist"
    let snapshotFileCache = distDir </> name

    let tmp = tmpDir </> name

    createDirectoryIfMissing True tmp

    -- tar up:
    snapshotName <- createTarBz2 cfg reg rev
    writeFile snapshotFileCache snapshotName

    -- run hack-nix to create cabal description 
    contents <- do
        _ <- rawSystemVerbose "tar" ["xfj", distDir </> snapshotName, "--strip-components=1"] (Just tmp)
        _ <- rawSystemVerbose "hack-nix" ["--to-nix"] (Just tmp)
        nixFiles' <- liftM (head . fst) $ globDir [compile (tmp </> "dist/*.nix")] "." 
        file <- maybe (fail "hack-nix --to-nix didn't write a dist/*.nix file")
                      return $ liftM (makeRelative tmp) $ listToMaybe nixFiles'
        readFile file
        
    return [contents]

-- gem implementation {{{1
-- runs also special gem command getting dependency information out of the snapshot .gem file
gemImpl _ rev cfg reg = do
  let repoDir = cfgRepoDir cfg
  let name = regionName reg
  let thisRepo = (cfgRepoDir cfg) </> name -- copied some lines below 
  let distDir = repoDir </> "dist"
  let snapshotFileCache = distDir </> name

  let r = fromJust $ repoFromMap (rOpts reg)
  let suffix = nameSuffix r rev

  -- recreate .gem file and cache
  -- build script putting ruby and gem in path:
  nixpkgsAll <- getEnv "NIXPKGS_ALL"
                  -- , last ist \n
  storePath <- liftM init $ runInteractiveProcess' 
                    "nix-build"
                    ["--no-out-link", "-A","ro.simpleEnv",nixpkgsAll] -- TODO make this configurabel ro. is my ruby overlay you can biuld -A simpleEnv from nixpkgs-overlay-ruby/default.nix instead
                    Nothing Nothing $ \(_,o,_) -> hGetContents o

  -- nixos must have multiple shells support script which fixes /etc/bashrc resetting PATH
  let rubyEnvScript = (storePath </> "bin" </> "ruby-env-simple")

  gemspecs <- liftM (head.fst) $ globDir [compile ("*.gemspec")] thisRepo
  case gemspecs of
    [] -> fail $ "no .gemspec file found in " ++ thisRepo
    [gemspecFile] -> do
          out <- runInteractiveProcess' 
                      rubyEnvScript
                      ["gem", "build", gemspecFile]
                      (Just thisRepo) Nothing $ \(_,o,_) -> hGetContents o
          let p = "  File: "
          case filter (p `isPrefixOf`) (lines out) of
            [line] -> do

                 let gemFile = thisRepo </> drop (length p) line
                 let gemVersion =  (takeWhile (/= '-')) . drop (length ".gem") $ reverse gemFile
                 let snapshotName = (name ++ "-" ++ gemVersion) ++ suffix ++ ".gem"
                 writeFile snapshotFileCache snapshotName
                 let target = distDir </> snapshotName
                 print $ "renaming created .gem " ++ gemFile ++ " to " ++ target
                 rename gemFile target

                 s <- source cfg target snapshotName
                 let src = s ++ ";"
                 
                 -- ask to generate region text and cache result in file
                 liftM lines $ runInteractiveProcess' 
                      rubyEnvScript
                      ["gem", "nixpkgsoverlay", target, src]
                      (Just thisRepo) Nothing $ \(_,o,_) -> hGetContents o
            _ -> error "no line with 'File: ' found in gem output"
    _ -> error "multiple .gemspec files found!"
