module Util where
import Control.Exception as E
import Data.Maybe
import System.Exit
import Control.Monad
import System.Directory
import System.Process
import System.IO



createDirectoryIfMissingVerbose :: FilePath -> IO ()
createDirectoryIfMissingVerbose dir = do
  e <- doesDirectoryExist dir
  when (not e) $ putStrLn ("creating dir " ++ dir) >> createDirectoryIfMissing True dir


rawSystemVerbose :: String -> [String] -> IO ExitCode
rawSystemVerbose app args = do
  let cmd = unwords (app:args) -- add quotes? 
  putStrLn $ "running " ++ cmd
  ph <- runProcess app args Nothing Nothing Nothing Nothing Nothing
  ec <- waitForProcess ph
  case ec of
    ExitSuccess -> return ec
    ExitFailure c -> do
      putStrLn $ cmd  ++  " failed with exit code " ++ (show c)
      return ec


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


-- copied from HUnit
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path f = do
    cur <- getCurrentDirectory
    setCurrentDirectory path
    finally f (setCurrentDirectory cur)

removeDevFiles :: FilePath -> IO ExitCode
removeDevFiles d = withCurrentDirectory d $ do
  rawSystemVerbose "rm" [ "-fr", "ID", "tags"] -- remove gnu id utils database and tags file

withTmpDir :: (FilePath -> IO ()) -> IO ()
withTmpDir act = do
  let d = "/tmp/nix_repsoitory_manager_tmp_dir"
  let clean = do
        de <- doesDirectoryExist d
        when de $ do
          rawSystemVerbose "chmod" [ "-R", "777",d]
          rawSystemVerbose "rm" [ "-fr", d] >> return ()
  clean
  createDirectoryIfMissing True d
  act d
  clean

data HashType = Sha256 | MD5
  deriving (Read)
instance Show HashType where
  show Sha256 = "sha256"
  show MD5 = "md5"


createHash :: FilePath -> HashType -> FilePath -> IO String
createHash dist hash dest = do
  h <-  runInteractiveProcess' "nix-hash" ["--type", (show hash),"--flat",  dist] Nothing Nothing $ \(_, out, _) -> do
      liftM ((!!0) . lines) $! hGetContents out
  writeFile dest $ "\"" ++ h ++ "\""
  return h

