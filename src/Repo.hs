{-# LANGUAGE CPP,ScopedTypeVariables #-}
module Repo where

#ifdef USE_INTERLUDE
#include "interlude.h"
import Interlude
#endif

import qualified Data.Map as M
import Util
import Control.Exception as E
import System.Process
import System.Environment
import System.IO
import System.Exit
import System.Locale
import Data.List
import Control.Monad
import GHC.Unicode
import System.Directory
import System.FilePath

#define OLDTIME

#ifdef OLDTIME
import System.Time
#else
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
#endif

type RepoConfig = M.Map String String


-- ========== repository types ======================================= 
data RepoInfo = RepoInfo
                 String -- name 
                 [String] -- groups so that you can update all belonging to the same group at once
                 (Maybe FilePath) -- subdirectory 
                 Repository
  deriving (Show,Read)
repoName :: RepoInfo -> String
repoName (RepoInfo n _ _ _) = n

data Repository = 
    DarcsRepo DarcsRepoData
  | SVNRepo SVNRepoData
  | CVSRepo CVSRepoData
  | GitRepo GitRepoData
  | BZRRepo BZRRepoData
  | MercurialRepo MercurialRepoData
  deriving (Show,Read)

data DarcsRepoData = DarcsRepoData String -- URL 
                                   (Maybe String) -- tag 
  deriving (Show,Read)

data SVNRepoData = SVNRepoData String -- URL 
                               (Maybe String) -- revision / Nothing = HEAD 
                               Bool -- --ignore-externals
  deriving (Show,Read)

data CVSRepoData = CVSRepoData String -- cvs root (eg :pserver:anonymous@synergy2.cvs.sourceforge.net:/cvsroot/synergy2)
                               String -- module
  deriving (Show,Read)

data GitRepoData = GitRepoData String -- URL, branch
                               (Maybe String) -- maybe branch name
                               Bool -- shallow (defaults to true, eg code.google.com does not support shallow clones yet)
  deriving (Show,Read)

data BZRRepoData = BZRRepoData String -- URL
  deriving (Show,Read)

data MercurialRepoData = MercurialRepoData String -- URL
                                           (Maybe String) -- maybe branch name

  deriving (Show,Read)

class Repo r where
  nameSuffix :: r -> String -> String

  parseFromConfig :: RepoConfig -> Maybe r -- Map -> Config 

  -- all files but the VCS files. This will be tarred up
  -- paths must be relative to repo directory
  sourceFiles :: r -- type dummy
                -> FilePath -- repo location
                -> IO [FilePath] -- list of relative filenames 

  repoGet ::  r -> String -> IO ExitCode
  repoGet = repoUpdate

  -- checks wether the repo is clean. This allows files to exist which do not belong to the repo.
  -- But it should return False if any file differs from the repo contents.. 
  -- (I've had an darcs pull issue once because of that)
  isRepoClean :: r -> FilePath -> IO Bool
  isRepoClean _ _ = return False

  repoUpdate ::  r -> String -> IO ExitCode
  repoUpdate = repoGet
  -- return revision identifier - should not contain spaces (or name = ..  has to be set when generating derivatrions.. )
  revId :: r -> FilePath -> IO String
  revId _ _ = -- poor default implementation just returning current time stamp 
#ifdef OLDTIME
      liftM (formatCalendarTime defaultTimeLocale "%F_%H-%M-%S") $ toCalendarTime =<< getClockTime
#else
      liftM (formatTime defaultTimeLocale "%F_%H-%M-%S") getCurrentTime
#endif

-- ============= instances ==============================================

instance Repo RepoInfo where
  nameSuffix (RepoInfo _ _ _ r) = nameSuffix r
  repoGet (RepoInfo _ _ _ r) = repoGet r
  isRepoClean (RepoInfo _ _ _ r) = isRepoClean r
  repoUpdate (RepoInfo _ _ _ r) = repoUpdate r
  revId (RepoInfo _ _ _ r) = revId r
  sourceFiles (RepoInfo _ _ _ r) = sourceFiles r
  parseFromConfig map' = do
     name <- M.lookup "name" map'
     repo <- parseFromConfig map'
     return $ RepoInfo name (maybe [] words $ M.lookup "groups" map') -- groups 
              (M.lookup "subdir" map')
             repo -- Repo 
           -- (maybe [Sha256] (map read . words) $ lookup "hash" map) [>Hash to use 

instance Repo Repository where
  nameSuffix (DarcsRepo r) = nameSuffix r
  nameSuffix (SVNRepo r) = nameSuffix r
  nameSuffix (CVSRepo r) = nameSuffix r
  nameSuffix (GitRepo r) = nameSuffix r
  nameSuffix (BZRRepo r) = nameSuffix r
  nameSuffix (MercurialRepo r) = nameSuffix r

  sourceFiles (DarcsRepo r) = sourceFiles r
  sourceFiles (SVNRepo r) = sourceFiles r
  sourceFiles (CVSRepo r) = sourceFiles r
  sourceFiles (GitRepo r) = sourceFiles r
  sourceFiles (BZRRepo r) = sourceFiles r
  sourceFiles (MercurialRepo r) = sourceFiles r
  parseFromConfig map' = do
    repoType <- M.lookup "type" map'
    case repoType of
      "darcs" -> parseFromConfig map' >>= return . DarcsRepo
      "svn" -> parseFromConfig map' >>= return . SVNRepo
      "cvs" -> parseFromConfig map' >>= return . CVSRepo
      "git" -> parseFromConfig map' >>= return . GitRepo
      "bzr" -> parseFromConfig map' >>= return . BZRRepo
      "hg" -> parseFromConfig map' >>= return . MercurialRepo
      _ -> Nothing

  repoGet (DarcsRepo r) = repoGet r
  repoGet (SVNRepo r) = repoGet r
  repoGet (CVSRepo r) = repoGet r
  repoGet (GitRepo r) = repoGet r
  repoGet (BZRRepo r) = repoGet r
  repoGet (MercurialRepo r) = repoGet r

  isRepoClean (DarcsRepo r) = isRepoClean r
  isRepoClean (SVNRepo r) = isRepoClean r
  isRepoClean (CVSRepo r) = isRepoClean r
  isRepoClean (GitRepo r) = isRepoClean r
  isRepoClean (BZRRepo r) = isRepoClean r
  isRepoClean (MercurialRepo r) = isRepoClean r

  repoUpdate (DarcsRepo r) = repoUpdate r
  repoUpdate (SVNRepo r) = repoUpdate r
  repoUpdate (CVSRepo r) = repoUpdate r
  repoUpdate (GitRepo r) = repoUpdate r
  repoUpdate (BZRRepo r) = repoUpdate r
  repoUpdate (MercurialRepo r) = repoUpdate r

  revId (DarcsRepo r) = revId r
  revId (SVNRepo r) = revId r
  revId (CVSRepo r) = revId r
  revId (GitRepo r) = revId r
  revId (BZRRepo r) = revId r
  revId (MercurialRepo r) = revId r




-- darcs implementation 
instance Repo DarcsRepoData where
  sourceFiles _ p = do
    files <- runInteractiveProcess' "darcs" ["show","files"] (Just p) Nothing $ \(_,o,_) -> hGetContents o
    return $ map (drop 2) $ filter (/= ".") $ lines files
    
  nameSuffix _ _ = "-darcs" -- rev is not helpful
  parseFromConfig map' = do 
        url <- M.lookup "url" map'
        return $ DarcsRepoData url $ M.lookup "tag" map'
  repoGet (DarcsRepoData url tag) dest = do
    removeDirectory dest -- darcs wants to create the directory itself 
    rawSystemVerbose "darcs" (["get", "--hashed", "--repodir=" ++ dest ] ++ ( maybe [] (\t -> ["--tag=" ++t]) tag) ++ [url]) (Just dest)

  repoUpdate (DarcsRepoData url _) dest = rawSystemVerbose "darcs" (["pull", "-a", "--repodir=" ++ dest, url ]) (Just dest)


  -- system ["darcs", "get", "--partial"]
  revId _ fp = do
    let p = "nrmtag"
    darcs <- findExecutable' "darcs"
    out <- runInteractiveProcess' darcs ["changes","--last=1"] (Just fp) Nothing $ \(_,o,_) -> hGetContents o
    let l = dropWhile isSpace . head . drop 1 . lines $ out
    if "tagged " `isPrefixOf` l  -- last patch is a tag, use that
      then return $ drop (length "tagged ") l
      else do -- create a new tag 
        out' <- runInteractiveProcess' darcs ["show","tags"] (Just fp) Nothing $ \(_,o,_) -> hGetContents o
        let nrm_tags = [ drop (length p) l' | l' <- map (dropWhile isSpace) (lines out'), p `isPrefixOf` l  ]
        let nextTag = case nrm_tags of
                [] -> 1
                xs -> (+(1 :: Int)) . last . sort . map read $ xs
        let newTag = p ++ (show nextTag)
        runInteractiveProcess' darcs ["tag", newTag] (Just fp) Nothing$ const (return ())
        return newTag
  isRepoClean _ fp = do
    darcs <- findExecutable' "darcs"
    p <- runProcess darcs ["whatsnew"] (Just fp) Nothing Nothing Nothing Nothing
    ec <- waitForProcess p
    case ec of
      ExitSuccess -> return False
      ExitFailure 1 -> return True
      ExitFailure ec' -> error $ "unkown darcs whatsnew exit code " ++ (show ec')

-- SVN implementation
instance Repo SVNRepoData where
  sourceFiles _ p = do
    -- is there an svn command ? svn list is a way too slow
    files <- getDirectoryContentsRecursive p
    return $ map (makeRelative p) $ filter (not . any (== ".svn") . splitDirectories) files 

  nameSuffix _ rev = "-svn-" ++ rev
  parseFromConfig map' = do 
    url <- M.lookup "url" map'
    return $ SVNRepoData url
                         (M.lookup "r" map') 
                         (maybe False (/= "false") (M.lookup "ignorne-externals" map'))
  repoGet (SVNRepoData url revision ignoreExternals) dest =
    rawSystemVerbose 
      "svn" (
          ["checkout", url] 
          ++ (if ignoreExternals then ["--ignore-externals"] else [])
          ++ (maybe ["-rHEAD"] (\r -> ["-r" ++ r]) revision) ++ [dest]
      ) 
      (Just dest)
  revId _ fp = do
    svn <- findExecutable' "svn"
    out <- runInteractiveProcess' svn ["info"] (Just fp) Nothing $ \(_,o,_) -> hGetContents o
    return $ let s = "Revision: " 
           in  (drop (length s) ) . head . filter (s `isPrefixOf`) . lines $ out
  isRepoClean _ fp = do
    svn <- findExecutable' "svn"
    out <- runInteractiveProcess' svn ["diff"] (Just fp) Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out

-- SVN implementation
-- TODO
instance Repo CVSRepoData where
  sourceFiles _ p = do
    -- is there an svn command ? svn list is a way too slow
    files <- getDirectoryContentsRecursive p
    return $ map (makeRelative p) $ filter (not . any (== ".cvs") . splitDirectories) files 

  nameSuffix _ rev = "-cvs-" ++ rev
  parseFromConfig map' = do 
    cvsRoot <- M.lookup "cvsRoot" map'
    module' <- M.lookup "module" map'
    return $ CVSRepoData cvsRoot module'
  repoGet (CVSRepoData cvsRoot module') dest = do
        _ <- rawSystemVerbose "cvs" [ "-z0", "-d", cvsRoot, "checkout", "-D", "NOW", module' ] (Just dest)
        rawSystemVerbose "sh" ["-c", "a=*; echo \"a is\" $a; mv $a/* .; rmdir $a"] (Just dest)
   -- this *should* work. However checking out hsql -> surprise, folder gone
    -- withCurrentDirectory (takeDirectory dest) $ do
      -- removeDirectory dest -- I guess git wants to create the directory itself 
      -- withCurrentDirectory ( takeDirectory dest ) $ do
        -- a <- rawSystemVerbose "cvs" [ "-z0", "-d", cvsRoot, "checkout", "-D", "NOW", "-d", (takeFileName dest), module' ]
        -- print "getting cvs sources complete finished"
        -- return a
      -- rawSystemVerbose "sh" $ ["-c", "a=*; echo \"a is\" $a; mv $a/* .; rmdir $a"];
  repoUpdate (CVSRepoData _ module') dest =
    rawSystemVerbose "cvs" ["update","-dA", moduleTail ] (Just dest)
    where moduleTail = case (dropWhile (not . (`elem` "/\\")) module') of
                      "" -> "."
                      ('/':xs) -> xs
                      _ -> error "unexpected char XYZ"


  isRepoClean _ _ = do
    print "isRepoClean to be implemented for cvs, returning True"
    return True


-- git implementation 
instance Repo GitRepoData where
  sourceFiles _ p = do
    files <- runInteractiveProcess' "git" ["ls-files"] (Just p) Nothing $ \(_,o,_) -> hGetContentsStrict o
    return $ filter (/= ".") $ lines files

  nameSuffix _ rev = "-git-" ++ take 5 rev
  parseFromConfig map' = do 
    url <- M.lookup "url" map'
    let mbBranch = M.lookup "branch" map'
    let mbShallow = M.lookup "shallow" map'

    return $ GitRepoData url mbBranch (maybe True (/= "false") mbShallow)
  repoGet (GitRepoData url mbBranch shallow) dest = do
    removeDirectory dest -- I guess git wants to create the directory itself 
    rawSystemVerbose "git" (["clone"] ++ (maybe [] (\b -> ["-b", b]) mbBranch) ++ (if shallow then ["--depth" ,"1" ] else []) ++ [ url, dest]) Nothing
    -- case ec of
    --   ExitFailure _ -> return ec
    --   -- if branch is given switch to it and setup remote tracking 
    --   _ -> case mbBranch of
    --       Just branch -> rawSystemVerbose "git" ["checkout", "-tb", branch, "origin/" ++ branch ] (Just dest)
    --       _ -> return ExitSuccess

  repoUpdate (GitRepoData _ _ _) dest =
    rawSystemVerbose "git" [ "pull"] (Just dest)
  revId _ fp = do
    git <- findExecutable' "git"
    out <- runInteractiveProcess' git ["rev-parse", "--verify","HEAD"] (Just fp) Nothing $ \(_,o,_) -> hGetContents o
    return $ take 7 $ head . lines $ out
  isRepoClean _ fp = do
    git <- findExecutable' "git"
    out <- runInteractiveProcess' git ["diff"] (Just fp) Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out


-- bzr implementation 
instance Repo BZRRepoData where
  sourceFiles _ _ = do
    error "bzr sourceFiles TODO"
    -- files <- runInteractiveProcess' "git" ["ls-files"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    -- return $ filter (/= ".") $ lines files


  nameSuffix _ rev = "-bzr-" ++ rev
  parseFromConfig map' = do 
    url <- M.lookup "url" map'
    return $ BZRRepoData url
  repoGet (BZRRepoData url) dest = do
    removeDirectory dest -- I guess bzr wants to create the directory itself 
    rawSystemVerbose "bzr" ["branch", url, dest] Nothing
  repoUpdate (BZRRepoData _) dest =
    rawSystemVerbose "bzr" [ "update"] (Just dest) -- TODO: fix url when it has changed 
  isRepoClean _ _ = do
    print "isRepoClean to be implemented for bzr, returning True"
    return True




-- Mercurial implementation 
instance Repo MercurialRepoData where
  sourceFiles _ p = do
    -- hg has a switch to change directory
    files <- runInteractiveProcess' "hg" ["manifest"] (Just p) Nothing $ \(_,o,_) -> hGetContents o
    return $ lines files
  nameSuffix _ rev = "-hg-" ++ rev
  parseFromConfig map' = do 
    url <- M.lookup "url" map'
    let mbBranch = M.lookup "branch" map'
    return $ MercurialRepoData url mbBranch
  repoGet (MercurialRepoData url mbBranch) dest = do
    removeDirectory dest -- I guess git wants to create the directory itself 
    ec <- rawSystemVerbose "hg" ["clone", url, dest] Nothing
    case mbBranch of
        Just branch -> rawSystemVerbose "hg" ["checkout", branch] (Just dest)
        Nothing -> return ec

  repoUpdate (MercurialRepoData _ _) dest = 
    rawSystemVerbose "hg" [ "pull"] (Just dest)
  revId _ fp = do
    hg <- findExecutable' "hg"
    out <- runInteractiveProcess' hg ["log", "-l1"] (Just fp) Nothing $ \(_,o,_) -> hGetContents o
    return $ take 7 $ drop 1 . dropWhile (/= ':') . drop 1 .  dropWhile (/= ':') . head . lines $ out
  isRepoClean _ p = do
    hg <- findExecutable' "hg"
    out <- runInteractiveProcess' hg ["diff"] (Just p) Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out



checkCleanness :: (Repo r) => FilePath -> r -> [Char] -> IO ()
checkCleanness thisRepo r n = do
      -- don't craete a new dist file if the repo is dirty
     isClean <- isRepoClean r thisRepo
     ignoreDirty <- (getEnv "IGNORE_DIRTY" >> return True) `E.catch` (\(_ :: SomeException) -> return  False)
     when ((not isClean) && (not ignoreDirty)) $ do
       error $ "the working directory of " ++ n ++" is dirty !, set IGNORE_DIRTY to ignore this test"

publishRepo :: RepoInfo -> FilePath -> IO String
publishRepo (RepoInfo _ _ _ _) distFileLocation = do
   distFile <- readFile distFileLocation
   let distFileName = takeFileName distFile
   -- upload server 
   rsync <- findExecutable' "rsync"
   runProcess' rsync [ "-cs", distFile, "nix@mawercer.de:public_html/repos/" ++ distFileName ] Nothing
   return distFile



repoFromMap :: RepoConfig -> Maybe Repository
repoFromMap = parseFromConfig
