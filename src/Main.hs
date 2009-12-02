-- packages:base,filepath,mtl,directory,process,time,old-locale
{-# LANGUAGE CPP,ScopedTypeVariables #-}

#define OLDTIME
-- OLDTIME is easier for bootstrapping

module Main where
import Control.Exception as E
import System.Process
import System.IO
import System.Locale
import GHC.IOBase
import System.Exit
import System.Cmd
import Data.List
import Data.Maybe
import Control.Monad
import GHC.Unicode
import System.Environment
import System.Directory
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

path :: String
path = "pkgs/misc/bleeding-edge-fetch-infos.nix"

printUsage :: IO ()
printUsage = do
  name <- getProgName
  putStrLn $ unlines
    [ "usage: "
    , ""
    , "  " ++ name ++ "  --snippets [git|hg|svn|cvs|bzr "
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

data HashType = Sha256 | MD5
  deriving (Read)
instance Show HashType where
  show Sha256 = "sha256"
  show MD5 = "md5"

-- ========== repository types ======================================= 
data RepoInfo = RepoInfo String -- name 
                 [String] -- groups so that you can update all belonging to the same group at once
                 Repository
  deriving (Show,Read)
repoName :: RepoInfo -> String
repoName (RepoInfo n _ _) = n

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
                -> IO ()

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
  createTarGz (RepoInfo _ _ r ) b c = createTarGz r b c
  repoGet (RepoInfo _ _ r) = repoGet r
  isRepoClean (RepoInfo _ _ r) = isRepoClean r
  repoUpdate (RepoInfo _ _ r) = repoUpdate r
  revId (RepoInfo _ _ r) = revId r
  parseFromConfig map' = do
     name <- lookup "name" map'
     repo <- parseFromConfig map'
     return $ RepoInfo name (maybe [] words $ lookup "groups" map') -- groups 
             repo -- Repo 
           -- (maybe [Sha256] (map read . words) $ lookup "hash" map) [>Hash to use 
  
instance Repo Repository where
  createTarGz (DarcsRepo r ) b c = createTarGz r b c
  createTarGz (SVNRepo r ) b c = createTarGz r b c
  createTarGz (CVSRepo r ) b c = createTarGz r b c
  createTarGz (GitRepo r ) b c = createTarGz r b c
  createTarGz (BZRRepo r ) b c = createTarGz r b c
  createTarGz (MercurialRepo r ) b c = createTarGz r b c
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

  createTarGz _ dir destFile = do
    d <- tempDir
    rawSystemVerbose "cp" [ "-r", dir, d ]
    removeDevFiles d
    rawSystemVerbose "rm" [ "-fr", (d </> "_darcs") ]
    -- I'm using darcs-git-import to browse history, so remove the _togit directory as well
    rawSystemVerbose "rm" [ "-fr", (d </> "_togit") ]
    rawSystemVerbose "sh" [ "-c", "[ -f *etup.*hs ] && rm -fr dist" ] -- clean.. else cabal will not be able to recognize that it should recompile the files -> trouble 
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()
  -- system ["darcs", "get", "--partial"]
  revId _ fp = withCurrentDirectory fp $ do
    let p = "nrmtag"
    darcs <- findExecutable' "darcs"
    out <- runInteractiveProcess' darcs ["changes","--last=1"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    let l = dropWhile isSpace . myHead "43" . drop 1 . lines $ out
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

-- SVN implementation
instance Repo SVNRepoData where
  parseFromConfig map' = do 
    url <- lookup "url" map'
    return $ SVNRepoData url $ lookup "r" map'
  repoGet (SVNRepoData url revision) dest = rawSystemVerbose "svn" $ ["checkout", url] ++ (maybe ["-rHEAD"] (\r -> ["-r" ++ r]) revision) ++ [dest]

  createTarGz _ dir destFile = do
    d <- tempDir
    rawSystemVerbose "cp" [ "-r", dir, d ]
    removeDevFiles d
    rawSystemVerbose "/bin/sh" [ "-c", "find " ++ (show d) ++ " -type d -name \".svn\" | xargs rm -fr " ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()
  revId _ fp = withCurrentDirectory fp $ do
    svn <- findExecutable' "svn"
    out <- runInteractiveProcess' svn ["info"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ let s = "Revision: " 
           in  (drop (length s) ) . myHead "abc" . filter (s `isPrefixOf`) . lines $ out
  isRepoClean _ = do
    svn <- findExecutable' "svn"
    out <- runInteractiveProcess' svn ["diff"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out

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

  createTarGz _ dir destFile = do
    d <- tempDir
    rawSystemVerbose "cp" [ "-r", dir, d ]
    removeDevFiles d
    rawSystemVerbose "/bin/sh" [ "-c", "find " ++ (show d) ++ " -type d -name \"CVS\" | rm -fr " ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()
  isRepoClean _ = do
    print "isRepoClean to be implemented for cvs, returning True"
    return True

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
  createTarGz _ dir destFile = do
    d <- tempDir
    rawSystemVerbose "cp" [ "-r", dir, d ]
    removeDevFiles d
    rawSystemVerbose "rm" [ "-fr", d </> ".git" ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()
  revId _ fp = withCurrentDirectory fp $ do
    git <- findExecutable' "git"
    out <- runInteractiveProcess' git ["rev-parse", "--verify","HEAD"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ myHead "2" . lines $ out
  isRepoClean _ = do
    git <- findExecutable' "git"
    out <- runInteractiveProcess' git ["diff"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out

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
  createTarGz _ dir destFile = do
    d <- tempDir
    removeDevFiles d
    rawSystemVerbose "cp" [ "-r", dir, d ]
    rawSystemVerbose "rm" [ "-fr", d </> ".bzr" ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()
  isRepoClean _ = do
    print "isRepoClean to be implemented for bzr, returning True"
    return True



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
  createTarGz _ dir destFile = do
    d <- tempDir
    rawSystemVerbose "cp" [ "-r", dir, d ]
    removeDevFiles d
    rawSystemVerbose "rm" [ "-fr", d </> ".hg" ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()
  revId _ fp = withCurrentDirectory fp $ do
    hg <- findExecutable' "hg"
    out <- runInteractiveProcess' hg ["log", "-l1"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ drop 1 . dropWhile (/= ':') . drop 1 .  dropWhile (/= ':') . myHead "3" . lines $ out
  isRepoClean _ = do
    hg <- findExecutable' "hg"
    out <- runInteractiveProcess' hg ["diff"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out

-- ========== helper functions ======================================= 

mySystem :: String -> IO ()
mySystem cmd = do status <- system cmd
                  case status of
                    ExitSuccess -> return ()
                    ExitFailure status'  -> error $ "cmd " ++ cmd ++ " failed with " ++ (show status')

createDirectoryIfMissingVerbose :: FilePath -> IO ()
createDirectoryIfMissingVerbose dir = do
  e <- doesDirectoryExist dir
  when (not e) $ putStrLn ("creating dir " ++ dir) >> createDirectoryIfMissing True dir


rawSystemVerbose :: String -> [String] -> IO ExitCode
rawSystemVerbose app args = do
  let cmd = unwords (app:args) -- add quotes? 
  putStrLn $ "running " ++ cmd
  ec <- rawSystem app args
  case ec of
    ExitSuccess -> return ec
    ExitFailure c -> do
      putStrLn $ cmd  ++  " failed with exit code " ++ (show c)
      return ec


data DoWorkAction = DWUpdate | DWPublish | DWUpdateThenPublish

runProcess' :: String -> [ String ] -> IO ()
runProcess' n args = do
  putStrLn $ "running " ++ n ++ (show args)
  h <- runProcess n args Nothing Nothing Nothing Nothing Nothing
  p <- waitForProcess h
  case p of
    ExitSuccess -> return ()
    ExitFailure ec -> error $ n ++ " " ++ (show args) ++ " exited with " ++ (show ec)

runInteractiveProcess' :: FilePath
                        -> [String]
                        -> Maybe FilePath
                        -> Maybe [(String, String)]
                        -> ((Handle, Handle, Handle) -> IO b)
                        -> IO b
runInteractiveProcess' cmd args mb_cwd mb_env f = do
  (i,o,e,p) <- runInteractiveProcess cmd args mb_cwd mb_env
  r <- f (i,o,e)
  ec <- waitForProcess p
  case ec of
    ExitSuccess -> return r
    ExitFailure e' -> error $ "command " ++ cmd ++ " " ++ show args ++ " failed with " ++ (show e')

findExecutable' :: [Char] -> IO FilePath
findExecutable' n = liftM (fromMaybe (error $ n ++ " not found in path")) $ findExecutable n

readFileStrict :: FilePath -> IO String
readFileStrict fp = do
    let fseqM [] = return [] 
        fseqM xs = last xs `seq` return xs
    fseqM =<< readFile fp

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
  createTarGz r thisRepo distFileName
  writeFile distFileLocation distFileName


publishRepo :: RepoInfo -> FilePath -> IO String
publishRepo (RepoInfo _ _ _) distFileLocation = do
   distFile <- readFile distFileLocation
   let distFileName = takeFileName distFile
   -- upload server 
   rsync <- findExecutable' "rsync"
   runProcess' rsync [ "-cs", distFile, "nix@mawercer.de:www/repos/" ++ distFileName ]
   return distFile

-- repoDir: the directory containing all repos 
doWork :: FilePath -> [NixFile] -> DoWorkAction -> [String] -> IO ()
doWork repoDir nixFiles cmd args = do

  let doWorkOnItem :: FilePath -> Item -> IO Item
      doWorkOnItem path i@(IReg (Region ind opts contents map') ) = do
        -- l1, l2 = the two lines = content of the region 
        let (l1,l2) = case contents of
              [] -> ( (BS.unpack ind) ++ "src = sourceFromHead (throw \"relative-distfile-path\")", "(throw \"not published\")")
              l@[a, b] -> (BS.unpack a, BS.unpack b)
        case lookup "name" map' of
          Nothing -> do
            warning $ " no name attr found in " ++ (show opts) ++ " in file " ++ path
            return i
          Just n -> do
            let groups = maybe [] words $ lookup "groups" map'

            if (any (`elem` args) ([n] ++ groups)) then
                -- item was selected 
              case parseFromConfig map' of
                Nothing -> do
                  warning $ "WARNING: missing attrs in section of file " ++ (show . (lookup "file")) map'
                  return i
                Just r@(RepoInfo n _ repo) -> do -- do the selected work 
                  let thisRepo = repoDir </> n -- copied some lines below 
                      distDir = repoDir </> "dist"
                      distFileNameF rId = addExtension (n ++ "-" ++ rId) ".tar.gz"
                      distFileF rId = distDir </> (distFileNameF rId)
                      revLog = repoDir </> (n ++ ".log") -- copied some lines below 
                      distFileLocation = distDir </> n
                      publish' = do
                        distFile <- publishRepo r distFileLocation
                        hash <- createHash distFile Sha256 (distFile ++ ".sha256")
                        return (l1, (BS.unpack ind) ++ "             (fetchurl { url = \"http://mawercer.de/~nix/repos/" ++ (takeFileName distFile) ++ "\"; sha256 = \"" ++ hash ++ "\"; });")
                      fstLine n = (BS.unpack ind) ++ "src = sourceFromHead " ++ BS.unpack ind ++ "\"" ++ (makeRelative distDir n) ++ "\""
                      update = do
                        updateRepoTarGz thisRepo r n distFileF distFileLocation
                        rev <- revId r thisRepo
                        withFile revLog AppendMode $ \h -> do
#ifdef OLDTIME
                          tStr <- liftM (calendarTimeToString) $ toCalendarTime =<< getClockTime
#else
                          tStr <- formatTime defaultTimeLocale "%c" =<< getCurrentTime)
#endif
                          hPutStrLn h (rev ++ " # " ++ tStr) >> hClose h
                        return rev

                  (c, d) <- case cmd of
                     DWUpdate -> do
                          rev <- update 
                          return (fstLine (distFileF rev), l2)
                     DWPublish -> publish'
                     DWUpdateThenPublish -> do
                          rev <- update
                          (_, x) <- publish'
                          return (fstLine (distFileF rev), x)
                  return $ IReg $ Region ind opts (map BS.pack [c,d]) map'

              -- unselected item: 
              else return i
      doWorkOnItem path a = return a
         
      doWorkOnFile :: NixFile -> IO ()
      doWorkOnFile file@(NixFile path items) = do
        file' <- liftM (NixFile path) $ mapM (doWorkOnItem path) items
        when (file' /= file) $ do
          -- write file with changed contents 
          writeNixFile path file'

  mapM_ doWorkOnFile nixFiles



createHash :: FilePath -> HashType -> FilePath -> IO String
createHash dist hash dest = do
  h <-  runInteractiveProcess' "nix-hash" ["--type", (show hash),"--flat",  dist] Nothing Nothing $ \(in', out, err) -> do
      liftM ((!!0) . lines) $! hGetContents out
  writeFile dest $ "\"" ++ h ++ "\""
  return h

tempDir :: IO FilePath
tempDir = do
  let d = "/tmp/nix_repsoitory_manager_tmp_dir"
  de <- doesDirectoryExist d
  when de $ rawSystemVerbose "rm" [ "-fr", d] >> return ()
  return d
  -- t <- getTemporaryDirectory
  -- l <- filterM ( not . doesDirectoryExist) $ [ t </> "nix_repository_manager" ++ (show i) | i <- [1..] ]
  -- return $ myHead "4" l

-- copied from HUnit
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path f = do
    cur <- getCurrentDirectory
    setCurrentDirectory path
    finally f (setCurrentDirectory cur)

removeDevFiles d = withCurrentDirectory d $ do
  rawSystemVerbose "rm" [ "-fr", "ID", "tags"] -- remove gnu id utils database and tags file

snippet options = putStrLn $ unlines $ map ("    " ++)
                    [ (BS.unpack regionStart) ++ ": " ++ options
                    , (BS.unpack regionEnd) ]
  

printSnippet "git" = snippet "{ name=\"?\"; type=\"git\"; url=\"git://..\"; [ groups = \"group1 group2\"; ]; }"
printSnippet "hg"  = snippet "{ name=\"?\"; type=\"hg\";  url=\"git://..\"; [ groups = \"group1 group2\"; ]; }"
printSnippet "svn" = snippet "{ name=\"?\"; type=\"svn\"; url=\"git://..\"; [ groups = \"group1 group2\"; ]; }"
printSnippet "cvs" = snippet "{ name=\"?\"; type=\"cvs\"; cvsRoot=\"...\"; module=\"module\" [ groups = \"group1 group2\"; ]; }"

main = do
  p <- getProgName
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
