{-# LANGUAGE CPP,ScopedTypeVariables #-}
module Repo where
import Util
import Control.Exception as E
import System.Process
import System.Environment
import System.IO
import System.Locale
import GHC.IOBase
import System.Exit
import Data.List
import Data.Maybe
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

type RepoConfig = [(String,String)] -- simply a list of tuples 


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
  deriving (Show,Read)

data CVSRepoData = CVSRepoData String -- cvs root (eg :pserver:anonymous@synergy2.cvs.sourceforge.net:/cvsroot/synergy2)
                               String -- module
  deriving (Show,Read)

data GitRepoData = GitRepoData String -- URL 
  deriving (Show,Read)

data BZRRepoData = BZRRepoData String -- URL 
  deriving (Show,Read)

data MercurialRepoData = MercurialRepoData String -- URL 

  deriving (Show,Read)

class Repo r where
  parseFromConfig :: RepoConfig -> Maybe r -- Map -> Config 

  -- filters unnecessary files (the way fetch* do this as well) 
  createTarGz :: r -- type dummy
                -> FilePath  -- repo directory 
                -> FilePath -- target .tar.gz 
                -> FilePath -- tmp dir
                -> IO ()

  -- remove vcs directories etc 
  clean :: r -> String -> IO ()
  clean _ _ = return ()  

  repoGet ::  r -> String -> IO ExitCode
  repoGet = repoUpdate

  -- checks wether the repo is clean. This allows files to exist which do not belong to the repo.
  -- But it should return False if any file differs from the repo contents.. 
  -- (I've had an darcs pull issue once because of that)
  isRepoClean :: r -> IO Bool
  isRepoClean _ = return False

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
  repoGet (RepoInfo _ _ _ r) = repoGet r
  isRepoClean (RepoInfo _ _ _ r) = isRepoClean r
  repoUpdate (RepoInfo _ _ _ r) = repoUpdate r
  revId (RepoInfo _ _ _ r) = revId r
  parseFromConfig map' = do
     name <- lookup "name" map'
     repo <- parseFromConfig map'
     return $ RepoInfo name (maybe [] words $ lookup "groups" map') -- groups 
              (lookup "subdir" map')
             repo -- Repo 
           -- (maybe [Sha256] (map read . words) $ lookup "hash" map) [>Hash to use 

  createTarGz r@(RepoInfo _ _ subdir _) dir destFile d = do
    let addSubdir p Nothing = p
        addSubdir p (Just s) = p </> s
    rawSystemVerbose "cp" [ "-r", addSubdir dir subdir, d ]
    removeDevFiles d
    clean r d
    -- Think about how to include license in all cases ?? 
    writeFile "README-this-repo" $ unlines ([
        "this distribution file was created by http://github.com/MarcWeber/nix-repository-manager",
        "Its source is: " ++ show r
        ] ++ (maybeToList (fmap ("containing only its subdirectory " ++) subdir))
      )
    rawSystemVerbose "tar"   [ "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "chmod" [ "-R", "777", d]
    rawSystemVerbose "rm"    [ "-fr", d ]
    return ()
  
instance Repo Repository where
  createTarGz (DarcsRepo r ) b c d = createTarGz r b c d
  createTarGz (SVNRepo r ) b c d = createTarGz r b c d
  createTarGz (CVSRepo r ) b c d = createTarGz r b c d
  createTarGz (GitRepo r ) b c d = createTarGz r b c d
  createTarGz (BZRRepo r ) b c d = createTarGz r b c d
  createTarGz (MercurialRepo r ) b c d = createTarGz r b c d
  parseFromConfig map' = do
    repoType <- lookup "type" map'
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
  parseFromConfig map' = do 
        url <- lookup "url" map'
        return $ DarcsRepoData url $ lookup "tag" map'
  repoGet (DarcsRepoData url tag) dest = do
    removeDirectory dest -- darcs wants to create the directory itself 
    rawSystemVerbose "darcs" $ ["get", "--hashed", "--repodir=" ++ dest ] ++ ( maybe [] (\t -> ["--tag=" ++t]) tag) ++ [url]

  repoUpdate (DarcsRepoData url _) dest = rawSystemVerbose "darcs" $ ["pull", "-a", "--repodir=" ++ dest, url ]

  clean _ d = do
    rawSystemVerbose "rm" [ "-fr", (d </> "_darcs") ]
    -- I'm using darcs-git-import to browse history, so remove the _togit directory as well
    rawSystemVerbose "rm" [ "-fr", (d </> "_togit") ]
    rawSystemVerbose "sh" [ "-c", "[ -f *etup.*hs ] && rm -fr dist" ] -- clean.. else cabal will not be able to recognize that it should recompile the files -> trouble 
    return ()

  -- system ["darcs", "get", "--partial"]
  revId _ fp = withCurrentDirectory fp $ do
    let p = "nrmtag"
    darcs <- findExecutable' "darcs"
    out <- runInteractiveProcess' darcs ["changes","--last=1"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    let l = dropWhile isSpace . head . drop 1 . lines $ out
    if "tagged " `isPrefixOf` l  -- last patch is a tag, use that
      then return $ drop (length "tagged ") l
      else do -- create a new tag 
        out' <- runInteractiveProcess' darcs ["show","tags"] Nothing Nothing $ \(_,o,_) -> hGetContents o
        let nrm_tags = [ drop (length p) l' | l' <- map (dropWhile isSpace) (lines out'), p `isPrefixOf` l  ]
        let nextTag = case nrm_tags of
                [] -> 1
                xs -> (+(1 :: Int)) . last . sort . map read $ xs
        let newTag = p ++ (show nextTag)
        runInteractiveProcess' darcs ["tag", newTag] Nothing Nothing$ const (return ())
        return newTag
  isRepoClean _ = do
    darcs <- findExecutable' "darcs"
    p <- runProcess darcs ["whatsnew"] Nothing Nothing Nothing Nothing Nothing
    ec <- waitForProcess p
    case ec of
      ExitSuccess -> return False
      ExitFailure 1 -> return True
      ExitFailure ec' -> error $ "unkown darcs whatsnew exit code " ++ (show ec')
  createTarGz = error "never called"

-- SVN implementation
instance Repo SVNRepoData where
  parseFromConfig map' = do 
    url <- lookup "url" map'
    return $ SVNRepoData url $ lookup "r" map'
  repoGet (SVNRepoData url revision) dest = rawSystemVerbose "svn" $ ["checkout", url] ++ (maybe ["-rHEAD"] (\r -> ["-r" ++ r]) revision) ++ [dest]

  clean _ d = do
    rawSystemVerbose "/bin/sh" [ "-c", "find " ++ (show d) ++ " -type d -name \".svn\" | xargs rm -fr " ]
    return ()

  revId _ fp = withCurrentDirectory fp $ do
    svn <- findExecutable' "svn"
    out <- runInteractiveProcess' svn ["info"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ let s = "Revision: " 
           in  (drop (length s) ) . head . filter (s `isPrefixOf`) . lines $ out
  isRepoClean _ = do
    svn <- findExecutable' "svn"
    out <- runInteractiveProcess' svn ["diff"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out
  createTarGz = error "never called"

-- SVN implementation
-- TODO
instance Repo CVSRepoData where
  parseFromConfig map' = do 
    cvsRoot <- lookup "cvsRoot" map'
    module' <- lookup "module" map'
    return $ CVSRepoData cvsRoot module'
  repoGet (CVSRepoData cvsRoot module') dest =
    withCurrentDirectory dest $ do
        rawSystemVerbose "cvs" [ "-z0", "-d", cvsRoot, "checkout", "-D", "NOW", module' ]
        rawSystemVerbose "sh" $ ["-c", "a=*; echo \"a is\" $a; mv $a/* .; rmdir $a"];
   -- this *should* work. However checking out hsql -> surprise, folder gone
    -- withCurrentDirectory (takeDirectory dest) $ do
      -- removeDirectory dest -- I guess git wants to create the directory itself 
      -- withCurrentDirectory ( takeDirectory dest ) $ do
        -- a <- rawSystemVerbose "cvs" [ "-z0", "-d", cvsRoot, "checkout", "-D", "NOW", "-d", (takeFileName dest), module' ]
        -- print "getting cvs sources complete finished"
        -- return a
      -- rawSystemVerbose "sh" $ ["-c", "a=*; echo \"a is\" $a; mv $a/* .; rmdir $a"];
  repoUpdate (CVSRepoData _ _) dest =
    withCurrentDirectory dest $ rawSystemVerbose "cvs" $ ["update"]

  clean _ d = do
    rawSystemVerbose "/bin/sh" [ "-c", "find " ++ (show d) ++ " -type d -name \"CVS\" | xargs rm -fr " ]
    return ()

  isRepoClean _ = do
    print "isRepoClean to be implemented for cvs, returning True"
    return True

  createTarGz = error "never called"

-- git implementation 
instance Repo GitRepoData where
  parseFromConfig map' = do 
    url <- lookup "url" map'
    return $ GitRepoData url
  repoGet (GitRepoData url) dest = do
    removeDirectory dest -- I guess git wants to create the directory itself 
    rawSystemVerbose "git" ["clone", url, dest]
  repoUpdate (GitRepoData _) dest =
    withCurrentDirectory dest $ rawSystemVerbose "git" [ "pull"]
  clean _ d = do
    rawSystemVerbose "rm" [ "-fr", d </> ".git" ]
    return ()

  revId _ fp = withCurrentDirectory fp $ do
    git <- findExecutable' "git"
    out <- runInteractiveProcess' git ["rev-parse", "--verify","HEAD"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ head . lines $ out
  isRepoClean _ = do
    git <- findExecutable' "git"
    out <- runInteractiveProcess' git ["diff"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out

  createTarGz = error "never called"

-- bzr implementation 
instance Repo BZRRepoData where
  parseFromConfig map' = do 
    url <- lookup "url" map'
    return $ BZRRepoData url
  repoGet (BZRRepoData url) dest = do
    removeDirectory dest -- I guess bzr wants to create the directory itself 
    rawSystemVerbose "bzr" ["branch", url, dest]
  repoUpdate (BZRRepoData _) dest =
    withCurrentDirectory dest $ rawSystemVerbose "bzr" [ "update"] -- TODO: fix url when it has changed 
  clean _ d = do
    rawSystemVerbose "rm" [ "-fr", d </> ".bzr" ]
    return ()
  isRepoClean _ = do
    print "isRepoClean to be implemented for bzr, returning True"
    return True

  createTarGz = error "never called"



-- Mercurial implementation 
instance Repo MercurialRepoData where
  parseFromConfig map' = do 
    url <- lookup "url" map'
    return $ MercurialRepoData url
  repoGet (MercurialRepoData url) dest = do
    removeDirectory dest -- I guess git wants to create the directory itself 
    rawSystemVerbose "hg" ["clone", url, dest]
  repoUpdate (MercurialRepoData  _) dest =
    withCurrentDirectory dest $ rawSystemVerbose "hg" [ "pull"]
  clean _ d = do
    rawSystemVerbose "rm" [ "-fr", d </> ".hg" ]
    return ()
  revId _ fp = withCurrentDirectory fp $ do
    hg <- findExecutable' "hg"
    out <- runInteractiveProcess' hg ["log", "-l1"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ drop 1 . dropWhile (/= ':') . drop 1 .  dropWhile (/= ':') . head . lines $ out
  isRepoClean _ = do
    hg <- findExecutable' "hg"
    out <- runInteractiveProcess' hg ["diff"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out

  createTarGz = error "never called"


checkCleanness :: (Repo r) => FilePath -> r -> [Char] -> IO ()
checkCleanness thisRepo r n = withCurrentDirectory thisRepo $ do
      -- don't craete a new dist file if the repo is dirty
     isClean <- isRepoClean r
     ignoreDirty <- (getEnv "IGNORE_DIRTY" >> return True) `E.catch` (\(_ :: SomeException) -> return  False)
     when ((not isClean) && (not ignoreDirty)) $ do
       error $ "the working directory of " ++ n ++" is dirty !, set IGNORE_DIRTY to ignore this test"

-- runs checkout / update, creates a new archive containing a revision in its name 
-- that archive file name is written to a file so that it's found again when publishing
updateRepoTarGz :: (Repo r) =>
                  FilePath
                  -> r
                  -> [Char]
                  -> (String -> FilePath)
                  -> FilePath
                  -> IO ()
updateRepoTarGz thisRepo r n distFileF distFileLocation = do
  de <- doesDirectoryExist thisRepo
  if de then do
         checkCleanness thisRepo r n
         putStrLn $ "\n\nupdating " ++ n
         fetch <- (getEnv "NO_FETCH" >> return False) `E.catch` (\(_ :: SomeException) -> return  True)
         if fetch then repoUpdate r thisRepo
                  else return $ ExitSuccess
       else do
         putStrLn $ "\n\nchecking out " ++ n
         createDirectoryIfMissing True thisRepo
         repoGet r thisRepo
  distFileName <- liftM distFileF (revId r thisRepo)
  withTmpDir $ \d -> do
    removeDirectory d -- it will be created by cp again 
    createTarGz r thisRepo distFileName d
  writeFile distFileLocation distFileName


publishRepo :: RepoInfo -> FilePath -> IO String
publishRepo (RepoInfo _ _ _ _) distFileLocation = do
   distFile <- readFile distFileLocation
   let distFileName = takeFileName distFile
   -- upload server 
   rsync <- findExecutable' "rsync"
   runProcess' rsync [ "-cs", distFile, "nix@mawercer.de:www/repos/" ++ distFileName ]
   return distFile

