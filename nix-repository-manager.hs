-- packages: base,filepath,mtl,directory,process
module Main where
import Control.Exception
import GHC.Handle
import GHC.IO
import System.Process
import System.IO
import GHC.IOBase
import System.Exit
import System.Cmd
import Data.List
import Control.Monad
import GHC.Unicode
import System.Environment
import System.Directory
import System.FilePath

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
        svn) put this version somwhere into the .tar.gz name
 -}

printUsage = putStrLn $ unlines
  [ "purpose: update repositiories with less effort providing hash sum for import within nix expressions automatically"
  , ""
  , "usage:"
  , "--update  list of names or groups to update or all (TODO)"
  , "--publish upload repostiroy to mawercer.de and write bleeding-edge-fetch-info/name.nix file"
  , "--update-then-publish list"
  , "--show-groups (TODO)"
  , "--show-repos (TODO)"
  , "--clean remove repositories no longer in config (TODO)"
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


-- ========== repository types ======================================= 
data RepoInfo = RepoInfo String -- name 
                 [String] -- groups (so that you can update all belonging to the same group at once
                 Repository
                 [HashType] -- which hash types to calculate 
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

  repoUpdate ::  r -> String -> IO ExitCode
  repoUpdate = repoGet

-- ============= instances ==============================================

instance Repo RepoInfo where
  createTarGz (RepoInfo _ _ r _ ) b c = createTarGz r b c
  repoGet (RepoInfo _ _ r _) = repoGet r
  repoUpdate (RepoInfo _ _ r _) = repoUpdate r
  parseFromConfig map = do name <- lookup "name" map
                           repo <- (parseFromConfig map)
                           return $ RepoInfo name (maybe [] words $ lookup "groups" map) -- groups 
                                   repo -- Repo 
                                  [Sha256, MD5]
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

  repoUpdate (DarcsRepo r) = repoUpdate r
  repoUpdate (SVNRepo r) = repoUpdate r
  repoUpdate (CVSRepo r) = repoUpdate r
  repoUpdate (BZRRepo r) = repoUpdate r
  repoUpdate (MercurialRepo r) = repoUpdate r

-- darcs implementation 
instance Repo DarcsRepoData where
  parseFromConfig map = do 
        url <- lookup "url" map
        return $ DarcsRepoData url $ lookup "tag" map
  repoGet (DarcsRepoData url tag) dest = do
    removeDirectory dest -- darcs wants to create the directory itself 
    rawSystemVerbose "darcs" $ ["get", "--partial", "--repodir=" ++ dest ] ++ ( maybe [] (\t -> ["--tag=" ++t]) tag) ++ [url]

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

-- SVN implementation
instance Repo SVNRepoData where
  parseFromConfig map = do 
    url <- lookup "url" map
    return $ SVNRepoData url $ lookup "r" map
  repoGet (SVNRepoData url revision) dest = rawSystemVerbose "svn" $ ["checkout", url] ++ (maybe ["-rHEAD"] (\r -> ["-r" ++ r]) revision) ++ [dest]

  createTarGz _ dir destFile = do
    d <- tempDir
    rawSystemVerbose "cp" [ "-r", dir, d ]
    rawSystemVerbose "/bin/sh" [ "-c", "find -type d " ++ (show d) ++ " -name \"*.svn\" | rm -fr " ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()

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
    rawSystemVerbose "/bin/sh" [ "-c", "find -type d " ++ (show d) ++ " -name \"*.cvs\" | rm -fr " ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, takeFileName d]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()

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

-- ========== helper functions ======================================= 
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

doWork h (Config repoDir repos) cmd args = do
  let reposFiltered = filter (\(RepoInfo n gs _ _) -> null args 
                                                    || any (`elem` args) (n:gs) 
                                                    || args == ["all"]) repos
  let f = case cmd of
            DWUpdate -> updateRepoTarGz
            DWPublish -> publish
            DWUpdateThenPublish -> (\r -> updateRepoTarGz r >> publish r)
  when (length reposFiltered < length args) $ print $ "warning, only updating " ++ (show $ map repoName reposFiltered)


  mapM_ f reposFiltered
  where updateRepoTarGz r@(RepoInfo n _ repo hash) = do
           -- update 
           let thisRepo = repoDir </> n -- copied some lines below 
           let distFileName = addExtension n ".tar.gz"
           let distDir = repoDir </> "dist"
           let distFile = distDir </> distFileName
           de <- doesDirectoryExist thisRepo
           if de then do
                 putStrLn $ "\n\nupdating " ++ n
                 repoUpdate repo thisRepo
               else do
                 putStrLn $ "\n\nchecking out " ++ n
                 createDirectoryIfMissing True thisRepo
                 repoGet repo thisRepo
           -- tar.gz 
           createDirectoryIfMissing True (distDir)
           createTarGz r thisRepo distFile
           sequence_ $ zipWith3 createHash (cycle [distFile]) hash [ distDir </> addExtension n (show h) | h <- hash ]
        publish r@(RepoInfo n _ repo hash) = do
           let thisRepo = repoDir </> n -- copied some lines above
           let distFileName = addExtension n ".tar.gz"
           let distDir = repoDir </> "dist"
           let distFile = distDir </> distFileName
           -- upload server 
           rawSystemVerbose "rsync" [ distFile, "nix@mawercer.de:www/repos/" ++ distFileName ]
           -- write .nix fetch info file
           sha256 <- liftM read $ readFile (distDir </> addExtension n (show Sha256))
           let str = unlines [
                      "  " ++ n ++ " = args: with args; fetchurl {"
                    , "    url = http://mawercer.de/~nix/repos/" ++ distFileName ++ ";"
                    , "    sha256 = " ++ show (sha256 :: String) ++ ";"
                    , "  };"
                    ]
           hPutStrLn h str
           putStrLn str

createHash :: FilePath -> HashType -> FilePath -> IO ()
createHash dist hash dest = do
  (in', out, err, ph) <- runInteractiveProcess "nix-hash" ["--type", (show hash),"--flat",  dist] Nothing Nothing
  h <- liftM ((!!0) . lines) $! hGetContents out
  ec <- waitForProcess ph
  mapM_ hClose [err, in']
  writeFile dest $ "\"" ++ h ++ "\""

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

-- myRead :: String -> IO Repo
-- myRead s = catch (const (print $ "could not parse config line" ++ s)) (return . read  $ s)
configFile :: [String] -> IO (FilePath, [String])
configFile ("--config":c:args) = return $ (c, args)
configFile args = liftM (\a -> (a </>  ".nix_repository_manager.conf" ,args) ) getHomeDirectory

main = do
  p <- getProgName
  h <- openFile ("/tmp/" ++ p) AppendMode
  hPutStrLn h "=  ======================================================="
  args <- getArgs
  home <- getHomeDirectory
  case args of 
    [] -> printUsage
    "--help":[] -> printUsage
    args -> do (configF, args) <- configFile args
               print "warning: there is no proper error reporting right now!"
               config <- parseConfig configF
               case args of
                ("--show-groups":_) -> putStrLn $ unwords $ nub $ concat $ map (\(RepoInfo _ gs _ _) -> gs) (repos config)
                ("--show-repos":_) -> putStrLn $ unwords $ map (\(RepoInfo name _ _ _) -> name) (repos config)
                ("--update":args) -> doWork h config DWUpdate args
                ("--publish":args) -> doWork h config DWPublish args
                ("--update-then-publish":args) -> doWork h config DWUpdateThenPublish args
                ("--clean":_) -> clean config
                ("--print-config":_) -> print config
                _ -> printUsage
