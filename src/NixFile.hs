{-# LANGUAGE CPP #-}
module NixFile where
import System.Directory
import Control.Monad.Error
import Data.Maybe
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

type ErrorWithIO e a = ErrorT e IO a

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

type Action = ErrorWithIO String [BS.ByteString] -- new region contens

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

repoAndNameFromMap :: BS.ByteString -> FilePath -> RepoConfig -> ErrorWithIO String (RepoInfo, String)
repoAndNameFromMap opts path map' = 
    let nameNotFound, missingAttrs :: String
        nameNotFound = "no name attr found in " ++ (show opts) ++ " in file " ++ path
        missingAttrs = "WARNING: missing attrs in section of file " ++ (show . (lookup "file")) map'
    in do
      name <- maybe (fail nameNotFound) return $ lookup "name" map'
      repo <- maybe (fail missingAttrs) return $ parseFromConfig map'
      return $ (repo, name)
whenSelected :: (Eq a, Monad m) =>
                [a] -> [a] -> a1 -> m a1 -> m a1
whenSelected args names contents f =
    if not (any (`elem` args) names )
      then return contents -- nothing to do 
    else f
{-
 a source region looks like this:

  # REGION AUTO UPDATE: { name="haxe"; type="cvs"; cvsRoot=":pserver:anonymous@cvs.motion-twin.com:/cvsroot"; module="haxe"; groups="haxe_group"; }
    content = "1";
  # END
-}


sourceRegion, hacknixRegion :: Region
sourceRegion = 
      region "# REGION AUTO UPDATE"  "# END" updateSourceRegionAction
updateSourceRegionAction :: ActionFun
updateSourceRegionAction (IRegionData ind opts contents map' _)
               path workAction args
               repoDir
               = do -- Either / Error monad 
                  (r, n') <- repoAndNameFromMap opts path map'
                  -- l1, l2 = the two lines = content of the region 
                  let extraInd = "             "
                      (l1,l2) = case contents of
                        [] -> ( (BS.unpack ind) ++ attrName ++ " = sourceFromHead (throw \"relative-distfile-path\")"
                              , (BS.unpack ind) ++ extraInd ++ "(throw \"source not not published yet: " ++ n' ++ "\");")
                        [a, b] -> (BS.unpack a, BS.unpack b)
                        _ -> error $ "wrong line count found in region in file " ++ path  -- should not occur 

                      groups = maybe [] words $ lookup "groups" map'
                      attrName = fromMaybe "src" $ lookup "srcAttrName" map'
                  
                  whenSelected args ([n'] ++ groups) contents $ do
                         do -- do the selected work 
                         let thisRepo = repoDir </> n' -- copied some lines below 
                             distDir = repoDir </> "dist"
                             distFileNameF rId = addExtension (n' ++ "-" ++ rId) ".tar.gz"
                             distFileF rId = distDir </> (distFileNameF rId)
                             -- revLog = repoDir </> (n' ++ ".log") -- copied some lines below 
                             distFileLocation = distDir </> n'
                             publish' = lift $ do
                               distFile <- publishRepo r distFileLocation
                               hash <- createHash distFile Sha256 (distFile ++ ".sha256")
                               return (l1, (BS.unpack ind) ++ extraInd ++ "(fetchurl { url = \"http://mawercer.de/~nix/repos/" ++ (takeFileName distFile) ++ "\"; sha256 = \"" ++ hash ++ "\"; });")
                             fstLine n'' = (BS.unpack ind) ++ attrName ++ " = sourceFromHead " ++ "\"" ++ (makeRelative distDir n'') ++ "\""
                             update = lift $ do
                               updateRepoTarGz thisRepo r n' distFileF distFileLocation
                               -- rev <- revId r thisRepo
                               -- read rev from file. Eg CVS is using current timestamp only
                               filename <- BS.readFile distFileLocation
                               let de = dropExtension . dropExtension
                               return $ de $ drop (1 {- the "-" -} + length n') $ takeFileName $ BS.unpack filename

                         (c, d) <- case workAction of
                            DWUpdate -> do
                                 rev <- update 
                                 return (fstLine (distFileF rev), l2)
                            DWPublish -> publish'
                            DWUpdateThenPublish -> do
                                 rev <- update
                                 (_, x) <- publish'
                                 return (fstLine (distFileF rev), x)
                         return $ map BS.pack [c,d]

{-
 a hacknix region looks like this:

  # REGION HACK_NIX: { name="haxe"; type="cvs"; cvsRoot=":pserver:anonymous@cvs.motion-twin.com:/cvsroot"; module="haxe"; groups="haxe_group"; }
  {
    ... hack nix cabal description
    src =  ..
  }

  where src is the same as pasted by sourceRegion
  The cabal description is created by hack-nix --to-nix
  # END

  -- TODO: run ./Setup dist instead of tar ! 
-}

hacknixRegion =
      region "# REGION HACK_NIX" "# END" action'
  where action' i@(IRegionData ind opts contents map' _)
               path workAction args
               repoDir
               = do
          let (_, srcOld, _) = splitC contents
              indent = map (ind `BS.append`)
              distDir = repoDir </> "dist"
              groups = maybe [] words $ lookup "groups" map'
          do -- Either / Error monad 
          (_, n') <- repoAndNameFromMap opts path map'
          whenSelected args ([n'] ++ groups) contents $ do
            srcContents <- updateSourceRegionAction (i {
                            rInd = ind `BS.append` (BS.pack "  "),
                            rContents = srcOld,
                            rMap = map' ++ [("srcAttrName", "srcFile")]
                            }) path workAction args repoDir
            distSrcFile <- lift $ liftM (BS.unpack) $ BS.readFile $ distDir </> n'
            let nameR = ( dropExtension . dropExtension . takeFileName) distSrcFile
            let hnDist = distDir </> nameR `addExtension` ".nix"
            (a,_,c) <- do
              -- run hack-nix to create cabal description 
              e <- lift $ doesFileExist hnDist
              when (not e) $ do
                lift $ withTmpDir $ \tmp -> withCurrentDirectory tmp $
                  do
                    rawSystemVerbose "tar" ["xfz", distSrcFile, "--strip-components=1"]
                    rawSystemVerbose "hack-nix" ["--to-nix"]
                    nixFiles' <- liftM (head . fst) $ globDir [compile "dist/*.nix"] "." 
                    file <- maybe (fail "hack-nix --to-nix didn't write a dist/*.nix file")
                                  return $ listToMaybe nixFiles'
                    copyFile file hnDist
                    copyFile file hnDist
              lift $ liftM (splitC . BS.lines) $ BS.readFile hnDist
            return $ (indent a) ++ srcContents ++ (indent c)

        -- separate the src Region from the rest 
        splitC :: [ BS.ByteString ] -> ([BS.ByteString], [BS.ByteString], [BS.ByteString])
        splitC list = case break (((BS.pack "srcFile") `BS.isPrefixOf`) . dropSpaces) list of
          (before, src:maybeSrc:rest) ->
            let contains' s = isJust $ BS.findSubstring (BS.pack s) maybeSrc
            in if contains' "throw \"" || contains' "fetchurl" then
                        (before, [src,maybeSrc], rest)
                    else
                        (before, [src], rest)
          (_,[]) ->
                -- try again, initial code. split at lines
                --   sha256="xx"
                --   url = "file:/..."
                -- line and replace that by src
                case break (((BS.pack "sha256") `BS.isPrefixOf`) . dropSpaces) list of
                  (before, src:urlEq:maybeSrc:rest) ->
                        -- the pretty printer sometimes puts the url below the "url ="
                        -- In this case 3 lines must be removed
                        let rest' = if BS.pack "url = " `BS.isSuffixOf` urlEq
                                      then rest
                                      else maybeSrc:rest
                        in (before, [] {- will be replaced anyway -}, rest')
                  _ -> error $ "unexpected break result"
                   -- ([],[],[]) -- no src? was empty. 
          r@(_,_) -> error $ "unexpected split result" ++ show r -- should never happen unless you mess up contents yourself. If this happens empty the contents and start from scratch 
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

startsWith :: BS.ByteString -> BS.ByteString -> Bool
startsWith a = BS.isPrefixOf a . dropSpaces

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
  lines <- liftM BS.lines $ BS.readFile file
  if (length lines > 5000) then do
      -- probably this is the hack-nix-db file! 
      putStrLn $ "WARNING! not parsing " ++ file ++ " too big (TODO figure out why it takes that long parsing such a file!)"
      return $ NixFile file [ IStr lines ]
    else return $ (NixFile file . parseF [] []) lines
  
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
            [ ind `BS.append` (regStart reg') `BS.append` (BS.pack ":") `BS.append` options
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

