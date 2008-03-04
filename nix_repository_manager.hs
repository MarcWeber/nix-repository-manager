-- packages: base,filepath,mtl,directory,process
module Main where
import GHC.Handle
import GHC.IO
import System.Process
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
    (TODO backup keep the last source in case of failure)
    repoDir/backup/nameA.tar.gz
                   nameA.md5
                   nameA.sha256

  status : it works (darcs) / experimental
  you want get reported all errors if there are any

  TODO: Start update jobs simultaniuously
        save logs (keeps some)
        make backup
 -}

printUsage = putStrLn $ unlines
  [ "purpose: update repositiories with less effort providing hash sum for import withing nix expressions automatically"
  , ""
  , "usage:"
  , "--update list of names or groups to update (TODO)"
  , "--show-groups (TODO)"
  , "--show-repos (TODO)"
  , "--clean remove repositories no longer in config (TODO)"
  , ""
  , "sample config start   ======================================================="
  , "/home/marc/managed_repos"
  , "[(\"name\",\"lib_X\"),(\"type\",\"darcs\"),(\"url\",\"http://...\"),(\"tag\",\"9.2 optional\"),(\"group\",\"happs\")]"
  , "[(\"name\",\"lib_X\"),(\"type\",\"svn\"),(\"url\",\"http://...\"),(\"r\",\"932 optional\"),(\"group\",\"groupA groupB\")]"
  , "sample config end   ======================================================="
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
  deriving (Show,Read)

data DarcsRepoData = DarcsRepoData String -- URL 
                                   (Maybe String) -- tag 
  deriving (Show,Read)

data SVNRepoData = SVNRepoData String -- URL 
                               (Maybe String) -- revision / Nothing = HEAD 
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
  parseFromConfig map = do
    repoType <- lookup "type" map
    case repoType of
      "darcs" -> parseFromConfig map >>= return . DarcsRepo
      "svn" -> parseFromConfig map >>= return . SVNRepo
      _ -> Nothing

  repoGet (DarcsRepo r) = repoGet r
  repoGet (SVNRepo r) = repoGet r

  repoUpdate (DarcsRepo r) = repoUpdate r
  repoUpdate (SVNRepo r) = repoUpdate r

instance Repo DarcsRepoData where
  createTarGz _ dir destFile = do
    d <- tempDir
    rawSystemVerbose "cp" [ "-r", dir, d ]
    rawSystemVerbose "rm" [ "-fr", (d </> "_darcs") ]
    rawSystemVerbose "tar" [  "cfz", destFile, "-C", takeDirectory d, "."]
    rawSystemVerbose "rm" [ "-fr", d ]
    return ()

  parseFromConfig map = do 
        url <- lookup "url" map
        return $ DarcsRepoData url $ lookup "tag" map
  repoGet (DarcsRepoData url tag) dest = do
    putStrLn $ "remvoing " ++ dest ++ " "++ url ++ " " ++ (show tag)
    removeDirectory dest -- darcs wants to create the directory itself 
    rawSystemVerbose "darcs" $ ["get", "--partial", "--repodir=" ++ dest ] ++ ( maybe [] (\t -> ["--tag=" ++t]) tag) ++ [url]

  repoUpdate (DarcsRepoData _ _) dest = rawSystemVerbose "darcs" $ ["pull", "-a", "--repodir=" ++ dest ]

  -- system ["darcs", "get", "--partial"]

instance Repo SVNRepoData where
 parseFromConfig map = do 
   url <- lookup "url" map
   return $ SVNRepoData url $ lookup "r" map
 repoGet (SVNRepoData url revision) dest = rawSystemVerbose "svn" $ ["checkout", url] ++ (maybe ["-rHEAD"] (\r -> ["-r" ++ r]) revision) ++ [dest]

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


clean (Config repoDir repos) = putStrLn "not yet implemented"
update (Config repoDir repos) args = do
  let reposFiltered = filter (\(RepoInfo n gs _ _) -> null args || any (`elem` args) (n:gs)) $ repos
  mapM_ updateRepoTarGz reposFiltered
  where updateRepoTarGz r@(RepoInfo n _ repo hash) = do
         -- update 
         let thisRepo = repoDir </> n
         let distDir = repoDir </> "dist"
         let distFile = distDir </> addExtension n ".tar.gz"
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

createHash :: FilePath -> HashType -> FilePath -> IO ()
createHash dist hash dest = do
  (in', out, err, ph) <- runInteractiveProcess "nix-hash" ["--type", (show hash), dist] Nothing Nothing
  h <- liftM ((!!0) . lines) $! hGetContents out
  ec <- waitForProcess ph
  mapM_ hClose [err, in']
  writeFile dest $ "\"" ++ h ++ "\""

tempDir :: IO FilePath
tempDir = return "/tmp/nix_repsoitory_manager_tmp_dir"
  -- t <- getTemporaryDirectory
  -- l <- filterM ( not . doesDirectoryExist) $ [ t </> "nix_repository_manager" ++ (show i) | i <- [1..] ]
  -- return $ head l

-- myRead :: String -> IO Repo
-- myRead s = catch (const (print $ "could not parse config line" ++ s)) (return . read  $ s)
configFile :: [String] -> IO (FilePath, [String])
configFile ("--config":c:args) = return $ (c, args)
configFile args = liftM (\a -> (a </>  ".nix_repository_manager.conf" ,args) ) getHomeDirectory

main = do
  args <- getArgs
  home <- getHomeDirectory
  case args of 
    [] -> printUsage
    "--help":[] -> printUsage
    args -> do (configF, args) <- configFile args
               config <- parseConfig configF
               print config
               case args of
                ("--show-groups":_) -> print $ concat $ map (\(RepoInfo _ gs _ _) -> gs) (repos config)
                ("--show-repos":_) -> print $ map (\(RepoInfo name _ _ _) -> name) (repos config)
                ("--update":args) -> update config args
                ("--clean":_) -> clean config
                _ -> printUsage
