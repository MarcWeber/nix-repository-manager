-- packages:base,filepath,mtl,directory,process,time,old-locale
{-# LANGUAGE CPP,ScopedTypeVariables #-}

#define OLDTIME
-- OLDTIME is easier for bootstrapping

module Main where
import Control.Monad.Error
import System.IO
import System.Environment
import Common
import NixFile
import Control.Monad
import GHC.IOBase
import System.Exit
import System.Cmd
import Data.List
import System.FilePath

#ifdef OLDTIME
import System.Time
#else
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
#endif


import NixFile

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
    , "  " ++ name ++ "  --snippet [git|hg|svn|cvs|bzr "
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

-- repoDir: the directory containing all repos 
doWork :: FilePath -> [NixFile] -> DoWorkAction -> [String] -> IO ()
doWork repoDir nixFiles' cmd args = do
        let doWorkOnItem :: FilePath -> Item -> IO Item
            doWorkOnItem _ i@(IStr _) = return i
            doWorkOnItem path' (IRegion region') = do
              let ir@(IRegionData _ _ cont _ reg') = region'
              res <- runErrorT $ (action reg') ir path' cmd args repoDir
              newC <- either (\s -> warning s >> return cont ) return res
              return $ IRegion $ ir{ rContents = newC }

            doWorkOnFile (NixFile path' items) = do
                items' <- mapM (doWorkOnItem path') items
                when (items' /= items) $ writeNixFile $ NixFile path' items'
            
        mapM_ doWorkOnFile nixFiles'


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
printSnippet _ = error "in print snippet" -- should never occur 

main :: IO ()
main = do
  args <- getArgs 
  case args of
    ["--snippet", typ] -> printSnippet typ
    (dir:rest) -> do
      parsed <- liftM (map snd) $ parseNixFiles dir
      repoDir <- getEnv "NIX_REPOSITORY_MANAGER_REPO_DIR"
      when (null repoDir) $ error "set env var NIX_REPOSITORY_MANAGER_REPO_DIR to your managedRepoDir, please!"
      let doWork' = doWork repoDir parsed
      case rest of
        ["--stats"] -> do
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
