-- run from current directory .cabal directory this way : dist/build/tests/tests

module Main where
import Test.HUnit.Base
import Test.HUnit.Text
import NixFile
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as BS

testParseNixFile = unsafePerformIO $ do -- grr
  parsed <- parseFileStrict "test-files/test.nix"
  return $ "parse nix file test1" ~: expected ~=? parsed

  where expected =  NixFile
          [ str ["{","#1"]
          , region   ""
                     " { name=\"haxe\"; type=\"cvs\"; cvsRoot=\":pserver:anonymous@cvs.motion-twin.com:/cvsroot\"; module=\"haxe\"; groups=\"haxe_group\"; }"
                     [ "  content = \"1\";" ]
                     [ ("name","haxe"), ("type","cvs"), ("cvsRoot",":pserver:anonymous@cvs.motion-twin.com:/cvsroot"),
                       ("module", "haxe"), ("groups", "haxe_group") ]

          , str ["#2"]
          , region "  "
                   " { name=\"other\"; type=\"git\"; url=\"git://other\"; }"
                   [ "  content = \"2\";" ]
                   [ ("name","other"), ("type", "git"), ("url","git://other") ]
          , str ["}"]
          ]
        region a b c = IReg . Region (BS.pack a) (BS.pack b) (map BS.pack c)
        str = IStr . map BS.pack

testReadOptions = TestList
  [ "1" ~: Right [] ~=? p "{}" 
  , "2" ~: Right [] ~=? p " {} "
  , "3" ~: Right [] ~=? p " {  } "
  , "4" ~: Right [("name","foobar")] ~=? p " { name =\"foobar\";  } "
  , "5" ~: Right [("name","foobar"),("repo","repX")] ~=? p " { name =\"foobar\"; repo =\"repX\"; } "
  , "6" ~: Right [("name","haxe"),("type","cvs"),("cvsRoot",":pserver:anonymous@cvs.motion-twin.com:/cvsroot"),("module","haxe"),("groups","haxe_group")]
            ~=?  p "{ name=\"haxe\"; type=\"cvs\"; cvsRoot=\":pserver:anonymous@cvs.motion-twin.com:/cvsroot\"; module=\"haxe\"; groups=\"haxe_group\"; }"
  ]
  where p = parseRepoConfig . BS.pack

main = do
  runTestTT $ TestList
    [ testParseNixFile
    , testReadOptions
    ]
