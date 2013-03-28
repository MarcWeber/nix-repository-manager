{-# LANGUAGE ScopedTypeVariables, CPP #-}
module Implementations where

#ifdef USE_INTERLUDE
#include "interlude.h"
import Interlude
#endif

import System.Time
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import Codec.Archive.Tar.Entry
import Data.Maybe
import qualified Data.Map as M
import Data.List
import Data.Char
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

getModTime :: FilePath -> IO EpochTime
getModTime path = do
  (TOD s _) <- getModificationTime path
  return $! fromIntegral s

-- strict version of packFileEntry2. Else too many open files *sick*
packFileEntry2 :: FilePath -- ^ Full path to find the file on the local disk
              -> TarPath  -- ^ Path to use for the tar Entry in the archive
              -> IO Entry
packFileEntry2 filepath tarpath = do
  mtime   <- getModTime filepath
  perms   <- getPermissions filepath
  file    <- openBinaryFile filepath ReadMode
  size    <- hFileSize file
  content <- liftM (BS.fromChunks.(:[])) $ BSS.hGetContents file
  return (simpleEntry tarpath (NormalFile content (fromIntegral size))) {
    entryPermissions = if executable perms then executableFilePermissions
                                           else ordinaryFilePermissions,
    entryTime = mtime
  }

createTarBz2 :: Config -> IRegionData -> String -> IO String -- basename of snapshot file
createTarBz2 cfg reg rev = addErrorContext "createTarBz2" $ do
  let name = regionName reg
  let r = fromJust $ repoFromMap (rOpts reg)
  let snapshotName = name ++ nameSuffix r rev ++ ".tar.bz2"
  let destFile = (cfgRepoDir cfg) </> "dist" </> snapshotName
  let (repoDir :: String) = (cfgRepoDir cfg) </> name
  sf <- sourceFiles r repoDir

  --  can't use packPaths because top dir nix_repository_manager is prefixed
  entries <- mapM (\p -> do
    let x = "nix_repository_manager" </> p
    let s = repoDir </> p
    isd <- doesDirectoryExist s
    let p = if isd then packDirectoryEntry else packFileEntry2
    p s (either error id (toTarPath isd x))
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
    let thisRepo = (cfgRepoDir cfg) </> name

    createDirectoryIfMissing True tmp

    -- tar up:
    snapshotName <- createTarBz2 cfg reg rev
    writeFile snapshotFileCache snapshotName

    -- run hack-nix to create cabal description
    contents <- do
        _ <- rawSystemVerbose "tar" ["xfj", distDir </> snapshotName, "--strip-components=1"] (Just tmp)
        print thisRepo
        setups <- filterM doesFileExist $ map (thisRepo </>) ["Setup.hs","Setup.lhs"]
        when (null setups) $ error "no Setup.*hs files found!"
        let setup = (takeFileName . head) setups
        _ <- rawSystemVerbose "ghc" ["--make", setup] (Just tmp)

        _ <- rawSystemVerbose "ghc" ["--make", setup] (Just tmp)
        mhn <- findExecutable "hack-nix"
        _ <- case mhn of
          Just hn -> rawSystemVerbose hn ["--to-nix"] (Just tmp)
          _ -> error "no hack-nix in PATH" -- should it be bulid ? takes longer than for gem
        nixFiles' <- liftM (head . fst) $ globDir [compile ("dist/*.nix")] tmp
        file <- maybe (fail "hack-nix --to-nix didn't write a dist/*.nix file")
                      return $ listToMaybe nixFiles'
        readFile file

    _ <- rawSystemVerbose "rm" ["-fr", tmp] Nothing

    let contentsWithoutSource = takeWhile (not . ("sha256 =" `isPrefixOf`) . dropWhile isSpace) . lines $ contents
    src <- source cfg (distDir </> snapshotName) snapshotName

    return $ contentsWithoutSource ++ [
              "  srcFile = " ++ src ++ ";"
            , "}"
            ]

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
                 let gemVersion = reverse . (takeWhile (/= '-')) . drop (length ".gem") $ reverse gemFile
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
