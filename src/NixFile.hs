{-# LANGUAGE CPP, ScopedTypeVariables #-}
module NixFile where
import qualified Data.Map as M
import Data.Function
import Control.Monad.Error
import Data.Maybe
import Data.Char
import Data.List
import System.IO
import System.FilePath.Glob


import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString.Char8 as BS

-- Nix file representation: {{{1

data NixFile = NixFile FilePath
                       Bool -- modified?
                       [ Item ]
  deriving (Show)


data Item = IStr [ BS.ByteString ]
          | IRegion IRegionData
  deriving (Show)
data IRegionData = IRegionData {
              sStart :: BS.ByteString,
              sEnd :: BS.ByteString,
              rInd :: String, -- indentation (spaces)
              rOptString :: BS.ByteString, -- OPTIONS 
              rContents :: [ BS.ByteString ], -- contents
              rOpts :: M.Map String String, -- map, parsed version of rOptString
              rFile :: FilePath -- path of file
              }
  deriving (Show)

-- two regions are the same if the start start line is the same
sameRegion :: IRegionData -> IRegionData -> Bool
sameRegion r1 r2 = ((==) `on` sStart) r1 r2

nixFileRegions :: NixFile -> [IRegionData]
nixFileRegions (NixFile _ _ list) = [i | IRegion i <- list ]

regionName :: IRegionData -> String
regionName i = M.findWithDefault (error $ "no key name in a region in " ++ rFile i) "name" (rOpts i)

-- write dirty nix files to disk
flushFiles :: [NixFile] -> IO [NixFile]
flushFiles = mapM writeNixFile

-- update region contents:
replaceRegionInFile :: IRegionData -> NixFile -> NixFile
replaceRegionInFile n (NixFile p m list) = 
  let sR (IStr _) =  False
      sR (IRegion i) = sameRegion i n
      replace s@(IStr _) = s
      replace i@(IRegion x) = if sameRegion x n then IRegion n else i
  in NixFile p (m || any sR list) $ map replace list

replaceRegionInFiles :: IRegionData -> [NixFile] -> [NixFile]
replaceRegionInFiles r = map (replaceRegionInFile r)
-- }}}1

isRegion :: Item -> Bool
isRegion (IRegion _) = True
isRegion _ = False

regions :: NixFile -> [ IRegionData ]
regions (NixFile _ _ items) = [ reg' | IRegion reg' <- items ]

dropSpaces :: BS.ByteString -> BS.ByteString
dropSpaces = BS.dropWhile isSpace

-- find all .nix files in directory 
nixFiles :: FilePath -> IO [ FilePath ]
nixFiles = liftM (head . fst) . globDir [compile "**/*.nix"]

namesAndGroups :: [ NixFile ] -> ( [String], [String] )
namesAndGroups list =
  let get name = catMaybes $ map (\(IRegionData _ _ _ _ _ map' _) -> M.lookup name map') (concatMap regions list)
  in ( nub $ get "name"
     , nub $ concatMap words $ get "groups")


-- read a nix file separating regions 
-- very simple parser to extract regions
parseFileStrict :: FilePath -> IO NixFile
parseFileStrict file = do
  source <- BS.readFile file
  either (fail . show) return $ parse parseNixFile file (BS.append source (BS.pack "\n")) -- force last line ending by \n

  where isSpaceNoN = (`elem` " \t")
        regionStart = try $ do
          ind <- many $ satisfy isSpaceNoN
          t <- string "# REGION "
          name <- many1 (satisfy (/= ':'))
          _ <- char ':'
          return (ind, t ++ name)

        regionEnd = try $ do
          sp <- many $ satisfy isSpaceNoN
          t <- string "# END"
          _ <- many $ satisfy isSpaceNoN
          return $ sp ++ t
          
        eol = char '\n' >> return ()

        parseNixFile = do
           -- many1 also works for empty files because \n is always appended look above
           items <- many1 $ choice [ textRegion , iRegion ]
           eof
           return $ NixFile file False items

        textRegion = try $ ( (liftM (IStr . map BS.pack) $ manyNot regionStart False) <?> "text region")

        l = many (satisfy (not . (`elem` "\n")))

        -- returns lines until parser not matches a line
        manyNot not' emptyOk = try $
          let notL emptyOk' = try $ do
                  bad <- lookAhead $ choice [try (not' >> return True), return False]
                  if bad
                    then fail "-"
                    else choice [ try (do line <- l
                                          eol
                                          more <- choice [try (notL True), return []]
                                          return $ line:more)
                                , if emptyOk' then return [] else fail "more lines expected" ]
          in if emptyOk then choice [ notL emptyOk, return [] ]
                else notL emptyOk

        iRegion = try $ do
          firstline <- lookAhead l
          (ind, start) <- regionStart
          let optstring = drop (1 + length (ind ++ start)) firstline
          spaces
          opts <- parseOptions <?> ("region options of " ++ start)
          eol
          contents <- manyNot regionEnd True <?> "region contents"
          end <- regionEnd <?> "region end"
          eol
          return $ IRegion $ IRegionData (BS.pack start) (BS.pack end) ind (BS.pack optstring) (map BS.pack contents) opts file

        -- parse options:
        parseOptions :: Parser (M.Map String String)
        parseOptions = do
          spaces
          _ <- char '{'
          contents <- many $ try parseKeyValue
          spaces
          _ <- char '}'
          _ <- many $ satisfy isSpaceNoN
          return $ M.fromList contents
        parseKeyValue :: Parser (String, String)        
        parseKeyValue = ( do
              spaces
              k <- parseKey
              spaces
              _ <- char '='
              spaces
              v <- parseValue
              spaces
              _ <- char ';'
              return (k,v)
              ) <?> "key=value pair"
        parseValue =
          choice [ quoted
                 , many1 (satisfy (not . isSpace)) ]
          <?> "value"
        parseKey =
          choice [ quoted
                 , many1 (satisfy (\c -> (not . isSpace) c && c /= '=') ) ]
          <?> "key"
        quoted = try ( char '"' >> do { c <- many parseQuotedChars; _ <- char '"'; return c; } )
        parseQuotedChars = try $ choice [ try ( char '\\' >> anyToken ) 
                                       , satisfy (/= '"') ]

-- write NixFile 
writeNixFile :: NixFile -> IO NixFile
writeNixFile (NixFile path _ items) = withFile path WriteMode $ \h -> do
  let writeItem (IStr l) = BS.hPutStr h $ BS.unlines l
      writeItem (IRegion (IRegionData start end ind options contents _ _)) = do
          BS.hPutStr h $ BS.unlines $
            [ (BS.pack ind) `BS.append` start `BS.append` (BS.pack ":") `BS.append` options
            ] ++ contents
            ++ [ (BS.pack ind) `BS.append` (BS.pack "# END") ]
  mapM_ writeItem items
  -- reset dirty flag:
  return $ NixFile path False items



