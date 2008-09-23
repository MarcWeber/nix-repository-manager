-- packages:base,filepath,mtl,directory,process,time,old-locale
module Main where
import Control.Exception as E
import GHC.Handle
import GHC.IO
import System.Process
import System.IO
import System.IO.Unsafe
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
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

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

path = "pkgs/misc/bleeding-edge-fetch-infos.nix"

printUsage = putStrLn $ unlines
  [ "purpose: update repositiories with less effort providing hash sum for import within nix expressions automatically"
  , ""
  , "usage:"
  , "<config File> [ <path to nixpkgs where to update bleeding-edge-fetch-info.nix> ]  one of the following options"
  , "--update  list of names or groups to update or all (TODO) (set NO_FETCH to only create dist/*.tar.gz)"
  , "--publish upload repostiroy to mawercer.de"
  , "--update-then-publish list"
  , "--show-groups (TODO)"
  , "--show-repos (TODO)"
  , "--clean remove repositories no longer in config (TODO)"
  , ""
  , "you can pass the path to the nixpkgs repo containing " ++ path ++ " as first arg,"
  , "then it will be updated automatically"
  , ""
  , ""
  , "sample config start   ======================================================="
  , "/home/marc/managed_repos"
  , "/etc/nixos/nixpkgs/pkgs/misc/bleeding-edge-fetch-info/"
  , "[(\"name\",\"lib_X\"),(\"type\",\"darcs\"),(\"url\",\"http://...\"),(\"tag\",\"9.2 optional\"),(\"group\",\"happs\")]"
  , "[(\"name\",\"lib_X\"),(\"type\",\"svn\"),(\"url\",\"http://...\"),(\"r\",\"932 optional\"),(\"group\",\"groupA groupB\")]"
  , "sample config end   ======================================================="
  , "where managed_repos is the path where the repositories will be stored (keep this user directory, so that nix can find them when you are installing"
  , " and  bleeding-edge-fetch-info is the directory where to put the name.nix fetchsrc{ ... } content"
  , ""
  , ""
  , " (\"hash\",\"Sha256\") or (\"hash\",\"Md5 Sha256\") is implemented right now as optional option, Sha256 is default"
  ]


data Config = Config { repoDir :: FilePath
                     , repos :: [ RepoInfo ] }
  deriving (Show)
emptyConfig = Config "" []

parseConfig :: FilePath -> IO Config
parseConfig file = do
  let parseLine :: Int -> String -> RepoInfo
      parseLine nr l = maybe (error ("errornous line " ++ (show nr))) id $ parseFromConfig (read l)
  (repoDir:lines) <- liftM lines $ readFile file
  return $ Config repoDir $ zipWith parseLine [1..] lines

data HashType = Sha256 | MD5
  deriving (Read)
instance Show HashType where
  show Sha256 = "sha256"
  show MD5 = "md5"

newtype AfterUpdateHook = AfterUpdateHook String deriving (Show, Read)
-- ========== repository types ======================================= 
data RepoInfo = RepoInfo String -- name 
                 [String] -- groups (so that you can update all belonging to the same group at once
                 Repository
                 (Maybe AfterUpdateHook)
  deriving (Show,Read)
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
  parseFromConfig :: [(String, String)] -> Maybe r -- Map -> Config 

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
  revId _ fp = -- poor default implementation just returning current time stamp 
      liftM (formatTime defaultTimeLocale "%F_%H-%M-%S") getCurrentTime

-- ============= instances ==============================================

instance Repo RepoInfo where
  createTarGz (RepoInfo _ _ r _ ) b c = createTarGz r b c
  repoGet (RepoInfo _ _ r _) = repoGet r
  isRepoClean (RepoInfo _ _ r _) = isRepoClean r
  repoUpdate (RepoInfo _ _ r _) = repoUpdate r
  revId (RepoInfo _ _ r _) = revId r
  parseFromConfig map = do name <- lookup "name" map
                           repo <- (parseFromConfig map)
                           let afterUpdateHook = lookup "afterUpdateHook" map >>= return . AfterUpdateHook
                           return $ RepoInfo name (maybe [] words $ lookup "groups" map) -- groups 
                                   repo -- Repo 
                                  afterUpdateHook
                                 -- (maybe [Sha256] (map read . words) $ lookup "hash" map) [>Hash to use 
  
instance Repo Repository where
  createTarGz (DarcsRepo r ) b c = createTarGz r b c
  createTarGz (SVNRepo r ) b c = createTarGz r b c
  createTarGz (CVSRepo r ) b c = createTarGz r b c
  createTarGz (GitRepo r ) b c = createTarGz r b c
  createTarGz (BZRRepo r ) b c = createTarGz r b c
  createTarGz (MercurialRepo r ) b c = createTarGz r b c
  parseFromConfig map = do
    repoType <- lookup "type" map
    case repoType of
      "darcs" -> parseFromConfig map >>= return . DarcsRepo
      "svn" -> parseFromConfig map >>= return . SVNRepo
      "cvs" -> parseFromConfig map >>= return . CVSRepo
      "git" -> parseFromConfig map >>= return . GitRepo
      "bzr" -> parseFromConfig map >>= return . BZRRepo
      "hg" -> parseFromConfig map >>= return . MercurialRepo
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
  parseFromConfig map = do 
        url <- lookup "url" map
        return $ DarcsRepoData url $ lookup "tag" map
  repoGet (DarcsRepoData url tag) dest = do
    removeDirectory dest -- darcs wants to create the directory itself 
    rawSystemVerbose "darcs" $ ["get", "--hashed", "--repodir=" ++ dest ] ++ ( maybe [] (\t -> ["--tag=" ++t]) tag) ++ [url]

  repoUpdate (DarcsRepoData url _) dest = rawSystemVerbose "darcs" $ ["pull", "-a", "--repodir=" ++ dest, url ]

  createTarGz _ dir destFile = do
    d <- tempDir
    rawSystemVerbose "cp" [ "-r", dir, d ]
    rawSystemVerbose "rm" [ "-fr", (d </> "_darcs") ]
    rawSystemVerbose "sh" [ "-c", "[ -f *etup.*hs ] && rm -fr dist" ] -- clean.. else cabal will not be able to recognize that it should recompile the files -> trouble 
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
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
        out <- runInteractiveProcess' darcs ["show","tags"] Nothing Nothing $ \(_,o,_) -> hGetContents o
        let nrm_tags = [ drop (length p) l | l <- map (dropWhile isSpace) (lines out), p `isPrefixOf` l  ]
        let nextTag = case nrm_tags of
                [] -> 1
                xs -> (+1) . last . sort . map read $ xs
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
      ExitFailure ec -> error $ "unkown darcs whatsnew exit code " ++ (show ec)

-- SVN implementation
instance Repo SVNRepoData where
  parseFromConfig map = do 
    url <- lookup "url" map
    return $ SVNRepoData url $ lookup "r" map
  repoGet (SVNRepoData url revision) dest = rawSystemVerbose "svn" $ ["checkout", url] ++ (maybe ["-rHEAD"] (\r -> ["-r" ++ r]) revision) ++ [dest]

  createTarGz _ dir destFile = do
    d <- tempDir
    rawSystemVerbose "cp" [ "-r", dir, d ]
    rawSystemVerbose "/bin/sh" [ "-c", "find " ++ (show d) ++ " -type d -name \"*.svn\" | rm -fr " ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()
  revId _ fp = withCurrentDirectory fp $ do
    svn <- findExecutable' "svn"
    out <- runInteractiveProcess' svn ["info"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ let s = "Revision: " 
             in  head . (drop (length s) ) . filter (s `isPrefixOf`) . lines $ out
  isRepoClean _ = do
    svn <- findExecutable' "svn"
    out <- runInteractiveProcess' svn ["diff"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out

-- SVN implementation
-- TODO
instance Repo CVSRepoData where
  parseFromConfig map = do 
    cvsRoot <- lookup "cvsRoot" map
    module' <- lookup "module" map
    return $ CVSRepoData cvsRoot module'
  repoGet (CVSRepoData cvsRoot module') dest =
    withCurrentDirectory dest $ do
        a <- rawSystemVerbose "cvs" [ "-z0", "-d", cvsRoot, "checkout", "-D", "NOW", module' ]
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
    rawSystemVerbose "/bin/sh" [ "-c", "find " ++ (show d) ++ " -type d -name \"CVS\" | rm -fr " ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()
  isRepoClean _ = do
    print "isRepoClean to be implemented for cvs, returning True"
    return True

-- git implementation 
instance Repo GitRepoData where
  parseFromConfig map = do 
    url <- lookup "url" map
    return $ GitRepoData url
  repoGet (GitRepoData url) dest = do
    removeDirectory dest -- I guess git wants to create the directory itself 
    rawSystemVerbose "git" ["clone", url, dest]
  repoUpdate (GitRepoData url) dest =
    withCurrentDirectory dest $ rawSystemVerbose "git" [ "pull"]
  createTarGz _ dir destFile = do
    d <- tempDir
    rawSystemVerbose "cp" [ "-r", dir, d ]
    rawSystemVerbose "rm" [ "-fr", d </> ".git" ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()
  revId _ fp = withCurrentDirectory fp $ do
    git <- findExecutable' "svn"
    out <- runInteractiveProcess' git ["rev-parse", "--verify","HEAD"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ head . lines $ out
  isRepoClean _ = do
    git <- findExecutable' "git"
    out <- runInteractiveProcess' git ["diff"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out

-- bzr implementation 
instance Repo BZRRepoData where
  parseFromConfig map = do 
    url <- lookup "url" map
    return $ BZRRepoData url
  repoGet (BZRRepoData url) dest = do
    removeDirectory dest -- I guess bzr wants to create the directory itself 
    rawSystemVerbose "bzr" ["branch", url, dest]
  repoUpdate (BZRRepoData url) dest =
    withCurrentDirectory dest $ rawSystemVerbose "bzr" [ "update"] -- TODO: fix url when it has changed 
  createTarGz _ dir destFile = do
    d <- tempDir
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
  parseFromConfig map = do 
    url <- lookup "url" map
    return $ MercurialRepoData url
  repoGet (MercurialRepoData url) dest = do
    removeDirectory dest -- I guess git wants to create the directory itself 
    rawSystemVerbose "hg" ["clone", url, dest]
  repoUpdate (MercurialRepoData url) dest =
    withCurrentDirectory dest $ rawSystemVerbose "hg" [ "pull"]
  createTarGz _ dir destFile = do
    d <- tempDir
    rawSystemVerbose "cp" [ "-r", dir, d ]
    rawSystemVerbose "rm" [ "-fr", d </> ".hg" ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()
  revId _ fp = withCurrentDirectory fp $ do
    hg <- findExecutable' "hg"
    out <- runInteractiveProcess' hg ["log", "-l1"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ dropWhile (/= ':') . dropWhile (/= ':') . head . lines $ out
  isRepoClean _ = do
    hg <- findExecutable' "hg"
    out <- runInteractiveProcess' hg ["diff"] Nothing Nothing $ \(_,o,_) -> hGetContents o
    return $ all isSpace out

-- ========== helper functions ======================================= 

mySystem :: String -> IO ()
mySystem cmd = do status <- system cmd
                  case status of
                    ExitSuccess -> return ()
                    ExitFailure status  -> error $ "cmd " ++ cmd ++ " failed with " ++ (show status)

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
clean (Config repoDir repos) = putStrLn "not yet implemented"

runProcess' :: String -> [ String ] -> IO ()
runProcess' n args = do
  putStrLn $ "running " ++ n ++ (show args)
  h <- runProcess n args Nothing Nothing Nothing Nothing Nothing
  p <- waitForProcess h
  case p of
    ExitSuccess -> return ()
    ExitFailure ec -> error $ n ++ " " ++ (show args) ++ " exited with " ++ (show ec)

runInteractiveProcess' cmd args mb_cwd mb_env f = do
  (i,o,e,p) <- runInteractiveProcess cmd args mb_cwd mb_env
  r <- f (i,o,e)
  ec <- waitForProcess p
  case ec of
    ExitSuccess -> return r
    ExitFailure e -> error $ "command " ++ cmd ++ " " ++ show args ++ " failed with " ++ (show e)

findExecutable' n = liftM (fromMaybe (error $ n ++ " not found in path")) $ findExecutable n

readFileStrict :: FilePath -> IO String
readFileStrict fp = do
    let fseqM [] = return [] 
        fseqM xs = last xs `seq` return xs
    fseqM =<< readFile fp

-- changing the number of lines will break replace (and the existing files
-- containing repo infos as well)
attr name distFileName sha256 = [
      "  " ++ name ++ " = args: with args; fetchurl { # " ++ ( formatTime defaultTimeLocale "%c" (unsafePerformIO getCurrentTime) )
    , "    url = \"http://mawercer.de/~nix/repos/" ++ distFileName ++ "\";"
    , "    sha256 = " ++ show (sha256 :: String) ++ ";"
    , "  };"
    ]


replace :: String -> String -> String -> String -> String
replace fileContents name distFileName sha256 = do
  unlines $ replaceadd (lines fileContents) (attr name distFileName sha256)
  where replaceadd ([last]) new = new ++ [last]
        replaceadd (l:ls) new
          | (isPrefixOf (name ++ " =") . dropWhile isSpace) l = new ++ drop 3 ls
          | otherwise = l:replaceadd ls new
addReplaceInFile (Just f) name distFileName sha256 = do
  let fp = (f ++ "/" ++ path)
  fc <- readFileStrict fp
  writeFile fp $ replace fc name distFileName sha256
addReplaceInFile _ _ _ _ = return ()

doWork h (Config repoDir repos) cmd args mbFileToUpdate = do
  let reposFiltered = filter (\(RepoInfo n gs _ _) -> null args 
                                                    || any (`elem` args) (n:gs) 
                                                    || args == ["all"]) repos
  let f = case cmd of
            DWUpdate -> updateRepoTarGz
            DWPublish -> publish
            DWUpdateThenPublish -> (\r -> updateRepoTarGz r >> publish r)
  print $ "updating repos " ++ (show $ map repoName reposFiltered)
  when (length reposFiltered < length args) $ print "!! no repos selected?"


  mapM_ f reposFiltered
  where updateRepoTarGz r@(RepoInfo n _ repo (afterUpdateHook)) = do
           -- update 
           let thisRepo = repoDir </> n -- copied some lines below 
           let revLog = repoDir </> (n ++ ".log") -- copied some lines below 
           let distFileName rId = addExtension (n ++ "-" ++ rId) ".tar.gz"
           let distDir = repoDir </> "dist"
           let distFileLocation = distDir </> n
           let checkCleanness = withCurrentDirectory thisRepo $ do
                    -- don't craete a new dist file if the repo is dirty
                   isClean <- isRepoClean r
                   ignoreDirty <- (getEnv "IGNORE_DIRTY" >> return True) `E.catch` (\_ -> return  False)
                   when ((not isClean) && (not ignoreDirty)) $ do
                     error $ "the working directory of " ++n ++" is dirty !, set IGNORE_DIRTY to ignore this test"

           checkCleanness
           de <- doesDirectoryExist thisRepo
           if de then do
                 putStrLn $ "\n\nupdating " ++ n
                 fetch <- (getEnv "NO_FETCH" >> return False) `E.catch` (\_ -> return  True)
                 if fetch then repoUpdate repo thisRepo
                          else return $ ExitSuccess
               else do
                 putStrLn $ "\n\nchecking out " ++ n
                 createDirectoryIfMissing True thisRepo
                 repoGet repo thisRepo
           -- afterUpdateHook 
           case afterUpdateHook of
            Just (AfterUpdateHook cmd) -> do
                    putStrLn $ "\n\nrunning afterUpdateHook" ++ n
                    withCurrentDirectory thisRepo $ mySystem cmd
            _ -> return ()
           checkCleanness
           -- tar.gz 
           createDirectoryIfMissing True (distDir)
           rId <- revId r thisRepo
           let distFile = distDir </> (distFileName rId)
           print $ "distfile loc :: " ++ distFileLocation
           writeFile distFileLocation distFile
           createTarGz r thisRepo distFile
           sha256 <- createHash distFile Sha256 (distFile ++ ".sha256")
           -- nix fetch info attrs
           let str = unlines $ attr n distFile sha256
           addReplaceInFile mbFileToUpdate n distFile sha256
           hPutStrLn h str
           putStrLn str
           h <- openFile revLog AppendMode
           t <- liftM (formatTime defaultTimeLocale "%c") getCurrentTime
           hPutStrLn h (rId ++ " # " ++ t) >> hClose h
        publish r@(RepoInfo n _ repo _) = do
           let thisRepo = repoDir </> n -- copied some lines above
           let distDir = repoDir </> "dist"
           let distFileLocation = distDir </> n
           distFile <- readFile distFileLocation
           let distFileName = takeFileName distFile
           -- upload server 
           rsync <- findExecutable' "rsync"
           runProcess' rsync [ "-cs", distFile, "nix@mawercer.de:www/repos/" ++ distFileName ]

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
  -- return $ head l

-- copied from HUnit
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path f = do
    cur <- getCurrentDirectory
    setCurrentDirectory path
    finally f (setCurrentDirectory cur)

main = do
  p <- getProgName
  h <- openFile ("/tmp/" ++ p) AppendMode
  hPutStrLn h "=  ======================================================="

  (configF:args) <- getArgs 
  (args, mbFileToUpdate) <- do
     case args of
      (f:args) -> do de <- doesDirectoryExist f 
                     print de
                     print f
                     if de then do 
                              putStrLn $ "directory " ++ f ++ " exists, will try updating " ++ path
                              return (args, Just f)
                           else return (f:args, Nothing)
      _ -> return (args, Nothing)
  
  home <- getHomeDirectory
  case args of 
    [] -> printUsage
    "--help":[] -> printUsage
    args -> do print "warning: there is no proper error reporting right now!"
               config <- parseConfig configF
               case args of
                ("--show-groups":_) -> putStrLn $ unwords $ nub $ concat $ map (\(RepoInfo _ gs _ _) -> gs) (repos config)
                ("--show-repos":xs) -> 
                  let fil = case xs of
                            [x] -> filter ((\(RepoInfo name _ _ _) -> (map toLower x) `isInfixOf` (map toLower name)))
                            [] -> id
                            _ -> error "too many arguments"
                  in putStrLn $ unwords $ map (\(RepoInfo name _ _ _) -> name) (fil $ repos config)
                ("--update":args) -> doWork h config DWUpdate args mbFileToUpdate
                ("--publish":args) -> doWork h config DWPublish args mbFileToUpdate
                ("--update-then-publish":args) -> doWork h config DWUpdateThenPublish args mbFileToUpdate
                ("--clean":_) -> clean config
                ("--print-config":_) -> print config
                _ -> printUsage
