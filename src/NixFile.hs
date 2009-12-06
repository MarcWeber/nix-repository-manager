{-# LANGUAGE CPP #-}
module NixFile where
import Data.Char
import Util
import System.FilePath
import Common
import Data.Maybe
import Data.List
import Data.Either
import System.IO
import System.FilePath.Glob
import Control.Monad
import Repo


import Text.Parsec
import Text.Parsec.ByteString (Parser)

import qualified Data.ByteString.Char8 as BS


-- very simple. Token is a recognize 
data NixFile = NixFile FilePath [ Item ]

type RegionContents = [BS.ByteString]

type ActionFun =
          ( IRegionData
          -> FilePath
          -> DoWorkAction
          -> [String] -- args, the region has to know whether it was selected 
          -> FilePath -- managedRepoDir
          -> Action
          )

data Action = Action (Either String -- failure
                        (IO (Either (IO Action) -- another action is required. I'd like to separate disk IO intensive tasks from nework tasks such as git clone later
                            [BS.ByteString]       -- new region contens
                            )
                       ))


data Region = Region {
  regStart :: BS.ByteString,
  regEnd :: BS.ByteString,
  action :: ActionFun  }
instance Show (Region) where
  show (Region start end _) = "Region " ++ show (start, end)

allRegions :: [Region]
allRegions = [ sourceRegion, hacknixRegion ]

region :: String -> String -> ActionFun -> Region
region a b = Region (BS.pack a) (BS.pack b)

sourceRegion, hacknixRegion :: Region
sourceRegion = 
      region "# REGION AUTO UPDATE"  "# END" action'
  where action' :: ActionFun
        action' (IRegionData ind opts contents map' _)
               path workAction args
               repoDir
               = Action $
            -- l1, l2 = the two lines = content of the region 
          case lookup "name" map' of
                Nothing -> do
                  Left $ "no name attr found in " ++ (show opts) ++ " in file " ++ path
                Just n -> do
                  let extraInd = "             "
                      (l1,l2) = case contents of
                        [] -> ( (BS.unpack ind) ++ "src = sourceFromHead (throw \"relative-distfile-path\")"
                              , (BS.unpack ind) ++ extraInd ++ "(throw \"source not not published yet: " ++ n ++ "\");")
                        [a, b] -> (BS.unpack a, BS.unpack b)
                        _ -> error $ "wrong line count found in region in file " ++ path  -- should not occur 

                      groups = maybe [] words $ lookup "groups" map'

                  if not (any (`elem` args) ([n] ++ groups))
                    then Right $ return $ Right contents -- nothing to do 
                    else
                        -- item was selected 
                      case parseFromConfig map' of
                        Nothing -> do
                          Left $ "WARNING: missing attrs in section of file " ++ (show . (lookup "file")) map'
                        Just r@(RepoInfo n' _ _ {- repo -}) -> Right $ do -- do the selected work 
                                let thisRepo = repoDir </> n' -- copied some lines below 
                                    distDir = repoDir </> "dist"
                                    distFileNameF rId = addExtension (n' ++ "-" ++ rId) ".tar.gz"
                                    distFileF rId = distDir </> (distFileNameF rId)
                                    -- revLog = repoDir </> (n' ++ ".log") -- copied some lines below 
                                    distFileLocation = distDir </> n'
                                    publish' = do
                                      distFile <- publishRepo r distFileLocation
                                      hash <- createHash distFile Sha256 (distFile ++ ".sha256")
                                      return (l1, (BS.unpack ind) ++ extraInd ++ "(fetchurl { url = \"http://mawercer.de/~nix/repos/" ++ (takeFileName distFile) ++ "\"; sha256 = \"" ++ hash ++ "\"; });")
                                    fstLine n'' = (BS.unpack ind) ++ "src = sourceFromHead " ++ "\"" ++ (makeRelative distDir n'') ++ "\""
                                    update = do
                                      updateRepoTarGz thisRepo r n' distFileF distFileLocation
                                      rev <- revId r thisRepo
                                      return rev

                                (c, d) <- case workAction of
                                   DWUpdate -> do
                                        rev <- update 
                                        return (fstLine (distFileF rev), l2)
                                   DWPublish -> publish'
                                   DWUpdateThenPublish -> do
                                        rev <- update
                                        (_, x) <- publish'
                                        return (fstLine (distFileF rev), x)
                                return $ Right $ map BS.pack [c,d]


hacknixRegion =
      region "# REGION HACK_NIX" "# END" action'
  where action' = error "TODO"

{-
 Str: everything this tool doesn't know about

 Region:

   # REGION AUTO UPDATE: OPTIONS
   ...
   # END

 
-}
data RegionSrc = RegionSrc
              BS.ByteString -- indentation (spaces)
              BS.ByteString -- OPTIONS 
              [ BS.ByteString ] -- contents
              [ (String, String) ] -- key name pairs. contents is written again. This is only used in queries
  deriving (Show, Eq)

data IRegionData = IRegionData {
              rInd :: BS.ByteString, -- indentation (spaces)
              rOpts :: BS.ByteString, -- OPTIONS 
              rContents :: [ BS.ByteString ], -- contents
              rMap :: [ (String, String) ], -- key name pairs. contents is written again. This is only used in queries
              reg :: Region -- interface to update things
              }

instance Eq IRegionData where
  (IRegionData ind opts contents _ _) == (IRegionData ind2 opts2 contents2 _ _) = 
      ind == ind2
      && opts == opts2
      && contents == contents2
  

data Item = IStr [ BS.ByteString ]
          | IRegion IRegionData

instance Eq Item where
  (IStr a) == (IStr b) =  a == b
  (IRegion a) == (IRegion b) = a == b
  _ == _ = False

  
isRegion :: Item -> Bool
isRegion (IRegion _) = True
isRegion _ = False

regions :: NixFile -> [ IRegionData ]
regions (NixFile _ items) = [ reg' | IRegion reg' <- items ]

dropSpaces :: BS.ByteString -> BS.ByteString
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
  let get name = catMaybes $ map (\(IRegionData _ _ _ map' _) -> lookup name map') (concatMap regions list)
  in ( nub $ get "name"
     , nub $ concatMap words $ get "groups")


-- read a nix file separating regions 
-- very simple parser to extract regions
parseFileStrict :: FilePath -> IO NixFile
parseFileStrict file = do
  liftM (NixFile file . parseF [] [] . BS.lines) $ BS.readFile file
  
  where
        parseF :: [ Item ] -- already parsed 
               -> [BS.ByteString] -- str. Maybe some more lines are concatenated
               -> [BS.ByteString] -- lines to be parsed
               -> [Item]
        parseF l s [] = l ++ [IStr s] -- no more lines
        parseF l s (next:rest) =
                  let spacesDropped = (dropSpaces next)
                      continueParsingText = parseF l (s ++ [next]) rest
                      parseThisRegion reg' =
                            let (region', rest') = parseRegion (regEnd reg') next [] rest
                                ind = BS.takeWhile isSpace next
                                (_, t) = BS.break (==':') spacesDropped
                                t' = BS.tail t
                                regItem = case parseRepoConfig t' of
                                   Left e -> error $ "error parsing options " ++ (show t') ++ " in file " ++ file ++ "\n" ++ e
                                   Right map' -> IRegionData ind t' region' ( ("file",file):map') reg'
                            in parseF (l ++ [ IStr s, IRegion regItem]) [] rest'
                  in
                    fromMaybe continueParsingText $
                        (listToMaybe . catMaybes) 
                        [ if (regStart reg') `BS.isPrefixOf` spacesDropped then
                            Just (parseThisRegion reg')
                          else Nothing
                        | reg' <- allRegions ]

        parseRegion :: BS.ByteString -> BS.ByteString -> [ BS.ByteString ] -> [ BS.ByteString ] -> ([BS.ByteString], [BS.ByteString])
        parseRegion _ start _ [] = error $ "error, no END comment found for section " ++  BS.unpack start
        parseRegion end start s (next:left) =
            if end `BS.isPrefixOf` (dropSpaces next) then
              (s, left)
            else parseRegion end start (s ++ [next]) left

-- write NixFile 
writeNixFile :: NixFile -> IO ()
writeNixFile (NixFile path items) = withFile path WriteMode $ \h -> do
  let writeItem (IStr l) = BS.hPutStr h $ BS.unlines l
      writeItem (IRegion (IRegionData ind options contents _ reg')) = do
          BS.hPutStr h $ BS.unlines $
            [ ind `BS.append` (regStart reg') `BS.append` (BS.pack ": ") `BS.append` options
            ] ++ contents
            ++ [ ind `BS.append` (regEnd reg') ]
  mapM_ writeItem items


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
                 , many1 (satisfy (\c -> (not . isSpace) c && c /= '=') ) ]
          <?> "key"
        quoted = try ( char '"' >> do { c <- many parseQuotedChars; char '"'; return c; } )
        parseQuotedChars = try $ choice [ try ( char '\\' >> anyToken ) 
                                       , satisfy (/= '"') ]

