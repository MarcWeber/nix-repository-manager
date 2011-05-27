{-# LANGUAGE ScopedTypeVariables #-}
module Util where
import GHC.MVar
import Types
import Control.Concurrent
import Data.Char
import Data.List
import Control.Exception as E
import Data.Maybe
import System.Exit
import Control.Monad
import System.Directory
import System.Process
import System.Posix.IO
import System.IO
import System.FilePath
import qualified System.IO.Unsafe as U



createDirectoryIfMissingVerbose :: FilePath -> IO ()
createDirectoryIfMissingVerbose dir = do
  e <- doesDirectoryExist dir
  when (not e) $ putStrLn ("creating dir " ++ dir) >> createDirectoryIfMissing True dir


rawSystemVerbose :: String -> [String] -> Maybe String -> IO ExitCode
rawSystemVerbose app args mb_cwd = do
  let cmd = unwords (app:args) -- add quotes? 
  putStrLn $ "running " ++ cmd
  ph <- runProcess app args mb_cwd Nothing Nothing Nothing Nothing
  ec <- waitForProcess ph
  case ec of
    ExitSuccess -> return ec
    ExitFailure c -> do
      putStrLn $ cmd  ++  " failed with exit code " ++ (show c)
      return ec


runProcess' :: String -> [ String ] -> Maybe FilePath -> IO ()
runProcess' n args mb_cwd = do
  putStrLn $ "running " ++ n ++ (show args)
  h <- runProcess n args mb_cwd Nothing Nothing Nothing Nothing
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
  putStrLn $ "running: " ++ (show (cmd:args)) ++ " in " ++ (show mb_cwd)
  (i,o,e,p) <- runInteractiveProcess cmd args mb_cwd mb_env
  r <- newEmptyMVar
  forkIO $ putMVar r =<< f (i,o,e)
  ec <- waitForProcess p
  case ec of
    ExitSuccess -> readMVar r
    ExitFailure e' -> error $ "command " ++ cmd ++ " " ++ show args ++ " failed with " ++ (show e')

findExecutable' :: [Char] -> IO FilePath
findExecutable' n = liftM (fromMaybe (error $ n ++ " not found in path")) $ findExecutable n

fseqM [] = return [] 
fseqM xs = last xs `seq` return xs

readFileStrict :: FilePath -> IO String
readFileStrict fp = fseqM =<< readFile fp

data HashType = Sha256 | MD5
  deriving (Read)
instance Show HashType where
  show Sha256 = "sha256"
  show MD5 = "md5"


createHash :: FilePath -> HashType -> FilePath -> IO String
createHash dist hash dest = addErrorContext "createHash" $ do
  h <-  runInteractiveProcess' "nix-hash" ["--type", (show hash),"--flat",  dist] Nothing Nothing $ \(_, out, _) -> do
      liftM ((!!0) . lines) $! hGetContents out
  writeFile dest $ "\"" ++ h ++ "\""
  return h

withHandle :: FilePath -> IOMode -> (Handle -> IO b) -> IO b
withHandle fn mode doWithHandle = do
    let lockMode WriteMode = WriteLock
        lockMode ReadMode = ReadLock
        lockMode _ = error "withHandle: unexpected"
    handle'<- openFile fn mode
    lockfd<- handleToFd handle' -- closes handle
    waitToSetLock lockfd (lockMode mode, AbsoluteSeek, 0, 0)
    putStrLn $ "done"
    handle2 <- fdToHandle lockfd
    r <- doWithHandle handle2
    hClose handle2
    return r

-- race condition - I don't care
newTempdir :: String -> IO FilePath
newTempdir prefix = do
  tmp <- getTemporaryDirectory
  let find' i = do
        let n = tmp </> (prefix ++ show i)
        de <- doesDirectoryExist n
        if de then find' (i+1)
              else return n
  tmpDir <- find' 0
  de <- doesDirectoryExist tmpDir
  when (not de) $ createDirectoryIfMissing True tmpDir
  return tmpDir

readConfig :: IO Config
readConfig = do
  d <- getHomeDirectory
  let f = d </> ".nix-repository-manager"
  ls <- readFile f
  return $ case filter (\l -> not (all isSpace l) && not ("#" `isPrefixOf`l)) (lines ls) of
    ["FORMAT:0.0", url, cmd, repoDir] -> Config url (read cmd) repoDir
    _ -> error $ "bad format in :" ++ f

-- taken from cabal
-- | List all the files in a directory and all subdirectories.
--
-- The order places files in sub-directories after all the files in their
-- parent directories. The list is generated lazily so is not well defined if
-- the source directory structure changes before the list is used.
--
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive topdir = recurseDirectories [""]
  where
    recurseDirectories :: [FilePath] -> IO [FilePath]
    recurseDirectories []         = return []
    recurseDirectories (dir:dirs) = U.unsafeInterleaveIO $ do
      (files, dirs') <- collect [] [] =<< getDirectoryContents (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')

      where
        collect files dirs' []              = return (reverse files, reverse dirs')
        collect files dirs' (entry:entries) | ignore entry
                                            = collect files dirs' entries
        collect files dirs' (entry:entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry:dirs') entries
            else collect (dirEntry:files) dirs' entries

        ignore ['.']      = True
        ignore ['.', '.'] = True
        ignore _          = False



addErrorContext :: String -> IO a -> IO a
addErrorContext s = handle (\(e::SomeException) -> putStrLn ("while " ++ s) >> throw e)

hGetContentsStrict h = fseqM =<< hGetContents h
  
