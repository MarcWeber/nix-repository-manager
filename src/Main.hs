-- packages:base,filepath,mtl,directory,process,time,old-locale
{-# LANGUAGE CPP,ScopedTypeVariables #-}

#define OLDTIME
-- OLDTIME is easier for bootstrapping

module Main where
import Control.Monad.Error
import Data.Function
import Control.Concurrent
import Util
import Repo
import System.Directory
import Data.Maybe
import Control.Exception as E
import ProcessPool
import Implementations
import Types
import System.IO
import System.Environment
import NixFile
import GHC.IO
import System.Cmd
import Data.List
import System.FilePath
import System.Exit
import qualified Data.Map as M

#ifdef OLDTIME
import System.Time
#else
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
#endif



import qualified Data.ByteString.Char8 as BS

{-

    directory Layout:

    repoDir/nameA ( Repository the way you get it using svn checkout, darcs get, etc, update is invoked here} /nameB
    repoDir/dist/nameA.tar.gz nameA.md5
                 nameA.sh256
    repoDir/backup/nameA.tar.gz
                   nameA.md5
                   nameA.sha256

  status : it works (darcs) / experimental
  you want get reported all errors if there are any

  TODO: Start update jobs simultaniuously
        save logs (keeps some)
        make backup so that your can switch back in case of build failures
        think about incremental source updates in general (maybe the way binary distributions are provided)
        when working with version control system supporting verion ids (such as
        svn, git, ..) put this version somwhere into the .tar.gz name

        !! change config file format to something more readable/ writable (Nix - Language ?)
           because you have to quote the " in afterUpdateHook yourself at the moment (FIXME)
 -}

myHead :: String -> [a] -> a
myHead _ l = head l

printUsage :: IO ()
printUsage = do
  name <- getProgName
  putStrLn $ unlines
    [ "usage: "
    , ""
    , "  " ++ name ++ "  --snippet [git|hg|svn|cvs|bzr|darcs] "
    , "        creates a snippet you can copy paste into the nix source code"
    , ""
    , "  " ++ name ++ " dir --update  [list]           "
    , "           checks out or updates the repositories"
    , ""
    , "  " ++ name ++ " dir  --publish [list]"
    , "           uploads the distribution file to your public location"
    , ""
    , "  " ++ name ++ " dir --update-then-publish [list]"
    , ""
    , "  " ++ name ++ " dir --stats          "
    , "           show num files, names, groups"
    , ""
    , "    [list] is a list of group names or names"
    , ""
    , ""
    , "  BUGS: maybe I should have used another language.."
    , "  "
    , "  "
    , "  If something went wrong you may have to export FORCE_REGION_UPDATE=1 to rewrite a region"
    ]

-- ========== helper functions ======================================= 

mySystem :: String -> IO ()
mySystem cmd = do status <- system cmd
                  case status of
                    ExitSuccess -> return ()
                    ExitFailure status'  -> error $ "cmd " ++ cmd ++ " failed with " ++ (show status')

-- changing the number of lines will break replace (and the existing files
-- containing repo infos as well)
attr :: [Char] -> FilePath -> String -> [[Char]]
attr name distFileName sha256 = 
  let
#ifdef OLDTIME
      tStr = unsafePerformIO $ liftM (calendarTimeToString) $ toCalendarTime =<< getClockTime
#else
      tStr = formatTime defaultTimeLocale "%c" (unsafePerformIO getCurrentTime) 
#endif
  in  [
        "  " ++ name ++ " = args: with args; fetchurl { # " ++ tStr
      , "    url = \"http://mawercer.de/~nix/repos/" ++ (takeFileName  distFileName) ++ "\";"
      , "    sha256 = " ++ show (sha256 :: String) ++ ";"
      , "  };"
      ]


warning :: String -> IO ()
warning = hPutStrLn stderr

-- map regions to tasks and run them in parallel
createTask :: Config -> FilePath -> MVar [NixFile] -> DoWorkAction -> IRegionData -> Task TaskType String String
createTask cfg tmpDir mv_nixfiles action reg = do

      let name = regionName reg
      let repoDir = cfgRepoDir cfg
      let thisRepo = repoDir </> name -- copied some lines below 
      let distDir = repoDir </> "dist"
      let revFile = repoDir </> "dist" </> (name ++ ".rev")
      let distFileLocation = distDir </> name

      let regions' = [ ("# REGION AUTO UPDATE", updateActionVCS autoUpdateImpl)
                    , ("# REGION HACK_NIX"    , updateActionVCS hackNixImpl)
                    , ("# REGION GEM"         , updateActionVCS gemImpl)
                    -- , ("# REGION PYTHON_PKG"  , updateActionVCS pythonImpl)
                    ]

          -- publish
          publish = do
                    let prog = head $ cfgUpload cfg
                        args file = map (\a -> if a == "FILE" then file else a) $ tail $ cfgUpload cfg
                    file <- liftM (distDir </>) $ readFile distFileLocation
                    runProcess' prog (args file) Nothing
                    return $ "uploaded :" ++ name

          --  1) update vcs repo
          --  2) call implementation updating region
          --  3) if publish is requested add new async task publishing source snapshot
          updateActionVCS impl addTask alsoPublish = do

                  let r = fromMaybe (error "invalid repo specification") $ repoFromMap (rOpts reg)

                  de <- doesDirectoryExist thisRepo
                  when (de) $ checkCleanness thisRepo r name

                  let set s = (getEnv s >> return True) `E.catch` (\(_ :: SomeException) -> return False)

                  fetch <- liftM not $ set "NO_FETCH"
                  forceRegionUpdate <- set "FORCE_REGION_UPDATE"
                  oldRev <- doesFileExist revFile >>= (\b -> if b then liftM (BS.unpack) (BS.readFile revFile) else return "") 
                  when fetch $ do
                        if de then do
                            putStrLn $ "updating " ++ name
                            code <- repoUpdate r thisRepo
                            case code of
                              ExitSuccess -> return ()
                              ExitFailure _ -> error $ "VCS: updating " ++ name ++ " failed"
                          else do
                            putStrLn $ "fetching " ++ name ++ " first time"
                            createDirectoryIfMissing  True thisRepo
                            code <- repoGet r thisRepo
                            case code of
                              ExitSuccess -> return ()
                              ExitFailure _ -> error $ "VCS: updating " ++ name ++ " failed"
                  rev <- revId r thisRepo
                  writeFile revFile rev
                  putStrLn $ "old rev: " ++ oldRev ++ " new rev: " ++ rev

                  -- update region contents and flush
                  when (rev /= oldRev || forceRegionUpdate) $ do
                    newRegionContents <- liftM (map ((rInd reg) ++)) $ impl tmpDir rev cfg reg
                    putStr "new region contents:"
                    print newRegionContents
                    modifyMVar_ mv_nixfiles $ flushFiles . replaceRegionInFiles (reg {rContents = map BS.pack newRegionContents})

                  when alsoPublish $ addTask (Task TTPublish ("publishing " ++ name) (const publish))
                  return $ "updated: " ++ name ++ " " ++ oldRev ++ " -> " ++ rev

          -- update then maybe publish
          update addTask alsoPublish = do
                  let updateAction = fromMaybe (error $ "unkown region :" ++ (BS.unpack (sStart reg)) ++ ", known regions: " ++ show (map fst regions')) $ lookup 
                                              (BS.unpack (sStart reg))
                                              regions'
                  updateAction addTask alsoPublish

      Task TTFetch
           ("updating " ++ name)
           (\at -> case action of
                      DWUpdate -> update at False
                      DWUpdateThenPublish -> update at True
                      DWPublish -> publish
           )

-- repoDir: the directory containing all repos 
doWork :: Config
        -> Int -- numCores
        -> [FilePath]  --  .nix files to process
        -> DoWorkAction -- actions to be taken on
        -> [String]   --  names or group names
        -> IO ()
doWork cfg numCores nixFiles' requestedActions args = do
        let -- should region be updated ?
            select reg =
              let names = [regionName reg] ++ words (fromMaybe "" (M.lookup "groups" (rOpts reg)))
              in any (`elem` args) names

        tmpDir <- newTempdir "nix-repository-manager-tmp-"
        print $ "tmp dir is: " ++ tmpDir

        putStr "parsing nix files .. "
        parsed <- mapM parseFileStrict nixFiles'
        putStrLn "done"

        nixFiles'' <- newMVar parsed

        --  affected regions:
        let regions'' = filter select $ concatMap nixFileRegions parsed

        -- if repo names are the same repo data must be the same as well
        let badDups a = let l = nubBy ((==) `on` rOpts) a 
                        in case l of
                            [i] -> Right i
                            _ -> Left l
        let (bad, good) = M.mapEither badDups $ M.fromListWith (++) [ ((sStart r, regionName r), [r]) | r <- regions'' ]
        when (not (M.null bad)) $ error $ "found multiple declarations for names " ++ show (M.keys bad) ++ " differing in options"


        -- run actions
        -- publishing (uploading) usually depends less on network latencies thus using one process only
        putStrLn $ "cores setting: " ++ (show numCores)
        results <- runConcurrently (M.fromList [(TTFetch, numCores), (TTPublish, 1)])
                        forkIO printProgress
                        (map (createTask cfg tmpDir nixFiles'' requestedActions) (M.elems good))
        results' <- takeMVar results
        mapM_ print results'
        when (any (either (const True) (const False)) results') $ exitWith $ ExitFailure 1
        

{-
snippet = error "TODO snippet command"
snippet :: String -> IO ()
snippet options = mapM_ showSnippet allRegions
  where showSnippet reg' = 
          putStrLn $ unlines $ map ("    " ++)
                          [ BS.unpack (regStart reg') ++ ": " ++ options
                          , BS.unpack (regEnd reg') ]

g = "[ groups = \"group1 group2\"; ]"

printSnippet :: [Char] -> IO ()
printSnippet "git" = snippet $ "{ name=\"?\"; type=\"git\"; url=\"\"; [branch = \"branchname\";] "++g++" }"
printSnippet "hg"  = snippet $ "{ name=\"?\"; type=\"hg\";  url=\"\"; [branch = \"branchname\";] "++g++" }"
printSnippet "svn" = snippet $ "{ name=\"?\"; type=\"svn\"; url=\"\"; "++g++"; }"
printSnippet "cvs" = snippet $ "{ name=\"?\"; type=\"cvs\"; cvsRoot=\"...\"; module=\"module\" "++g++" }"
printSnippet "bzr" = snippet $ "{ name=\"?\"; type=\"bzr\"; url=\"...\"; "++g++" }"
printSnippet "darcs" = snippet $ "{ name=\"?\"; type=\"darcs\"; url=\"...\"; "++g++" }"
printSnippet _ = error "in print snippet" -- should never occur 
-}

main :: IO ()
main = do
  args <- getArgs 
  config <- readConfig
  case args of
    -- ["--snippet", typ] -> printSnippet typ
    (numCores:dir:rest) -> do
      files <- nixFiles dir
      -- TODO get value from ~/.nix-profile/config.nix
      repoDir <- getEnv "NIX_REPOSITORY_MANAGER_REPO_DIR"
      when (null repoDir) $ error "set env var NIX_REPOSITORY_MANAGER_REPO_DIR to your managedRepoDir, please!"
      let distDir = repoDir + "/dist"
      de <- doesDirectoryExist distDir
      when (!de) $ error $ "run  mkdir -p " ++ distDir ++ ", please!"
      let doWork' = doWork config (read numCores) files
      case rest of
        ["--stats"] -> do
           parsed <- mapM parseFileStrict files
           let (names, groups) = namesAndGroups parsed
           putStrLn $ "files: " ++ show (length parsed)
           putStrLn $ "found sections: " ++ (show . sum) (map (length . regions) parsed)
           putStrLn $ "names: " ++ show names
           putStrLn $ "groups: " ++ show groups
        ("--update":list) -> doWork' DWUpdate list
        ("--publish":list) -> doWork' DWPublish list
        ("--update-then-publish":list) -> doWork' DWUpdateThenPublish list

        _ -> printUsage
    _ -> printUsage
