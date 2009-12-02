module NixFile where
import Data.Maybe
import Data.List
import Data.Either
import System.IO
import Data.Char (isSpace)
import Control.Monad
import System.FilePath.Glob
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS


import Text.Parsec
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle)
import Data.Char

-- import qualified Data.ByteString.Char8 as BS


-- very simple. Token is a recognize 
data NixFile = NixFile FilePath [ Item ]
  deriving (Show, Eq)

regionStart = BS.pack "# REGION AUTO UPDATE" 
regionEnd   = BS.pack "# END"

{-
 Str: everything this tool doesn't know about

 Region:

   # REGION AUTO UPDATE: OPTIONS
   ...
   # END

 
-}
data Region = Region
              BS.ByteString -- indentation (spaces)
              BS.ByteString -- OPTIONS 
              [ BS.ByteString ] -- contents
              [ (String, String) ] -- key name pairs. contents is written again. This is only used in queries
  deriving (Show, Eq)

data Item = IStr [ BS.ByteString ]
          | IReg Region
  deriving (Show, Eq)

regionOptions (Region _ _ _ opts) = opts

isRegion (IReg _) = True
isRegion _ = False

regions :: NixFile -> [ Region ]
regions (NixFile _ items) = [ reg | IReg reg <- items ]

dropSpaces = BS.dropWhile isSpace

-- find all .nix files in directory 
nixFiles :: FilePath -> IO [ FilePath ]
nixFiles = liftM (head . fst) . globDir [compile "**/*.nix"]

parseNixFiles :: FilePath -> IO [(FilePath, NixFile)]
parseNixFiles dir = do
  files <- nixFiles dir
  parsed <- mapM parseFileStrict files
  return $ zip files parsed

namesAndGroups :: [ NixFile ] -> ( [String], [String] )
namesAndGroups list =
  let get name = catMaybes $ map (\(Region _ _ _ map) -> lookup name map) (concatMap regions list)
  in ( nub $ get "name"
     , nub $ concatMap words $ get "groups")


-- read a nix file separating regions 
-- very simple parser to extract regions
parseFileStrict :: FilePath -> IO NixFile
parseFileStrict file = do
  liftM (NixFile file . parse [] [] . BS.lines) $ BS.readFile file
  
  where
        parse :: [ Item ] -- already parsed 
               -> [BS.ByteString] -- str. Maybe some more lines are concatenated
               -> [BS.ByteString] -- lines to be parsed
               -> [Item]
        parse l s [] = l ++ [IStr s] -- no more lines
        parse l s (next:rest) =
                  let spacesDropped = (dropSpaces next)
                  in
                    if regionStart `BS.isPrefixOf` spacesDropped then
                        let (region, rest') = parseRegion next [] rest
                            ind = BS.takeWhile isSpace next
                            (_, t) = BS.break (==':') spacesDropped
                            t' = BS.tail t
                            regItem = case parseRepoConfig t' of
                               Left e -> error $ "error parsing options " ++ (show t') ++ " in file " ++ file ++ "\n" ++ e
                               Right map -> Region ind t' region( ("file",file):map)
                        in parse (l ++ [ IStr s, IReg regItem]) [] rest'
                    else parse l (s ++ [next]) rest
        parseRegion :: BS.ByteString -> [ BS.ByteString ] -> [ BS.ByteString ] -> ([BS.ByteString], [BS.ByteString])
        parseRegion start s [] = error $ "error, no END comment found for section " ++  BS.unpack start
        parseRegion start s (next:left) =
            if regionEnd `BS.isPrefixOf` (dropSpaces next) then
              (s, left)
            else parseRegion start (s ++ [next]) left

-- write NixFile 
writeNixFile :: FilePath -> NixFile -> IO ()
writeNixFile path (NixFile _ items) = withFile path WriteMode $ \h -> do
  let writeItem (IStr l) = BS.hPutStr h $ BS.unlines l
      writeItem (IReg (Region ind options contents _)) = do
          BS.hPutStr h $ BS.unlines $
            [ ind `BS.append` regionStart `BS.append` (BS.pack ": ") `BS.append` options
            ] ++ contents
            ++ [ ind `BS.append` regionEnd ]
  mapM_ writeItem items


type RepoConfig = [(String,String)] -- simply a list of tuples 

parseRepoConfig :: BS.ByteString -> (Either String RepoConfig)
parseRepoConfig = either (Left . show) Right . parse parser ""
  where parser :: Parser RepoConfig
        parser = do
          spaces
          char '{'
          contents <- many $ try parseKeyValue
          spaces
          char '}' >> spaces >> eof
          return contents
        parseKeyValue :: Parser (String, String)        
        parseKeyValue = ( do
              spaces
              k <- parseKey
              spaces
              char '='
              spaces
              v <- parseValue
              spaces
              char ';'
              return (k,v)
              ) <?> "key=value pair"
        parseValue =
          choice [ quoted
                 , many1 (satisfy (not . isSpace)) ]
          <?> "value"
        parseKey =
          choice [ quoted
                 , many1 (satisfy (\c -> (\c -> (not . isSpace) c && c /= '=') c && c /= '=')) ]
          <?> "key"
        quoted = try ( char '"' >> do { c <- many parseQuotedChars; char '"'; return c; } )
        parseQuotedChars = try $ choice [ try ( char '\\' >> anyToken ) 
                                       , satisfy (/= '"') ]

