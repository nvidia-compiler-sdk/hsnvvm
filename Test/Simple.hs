module Main (main) where

import Foreign.LibNVVM
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Test.Framework as TF (Test, defaultMain, testGroup)

main :: IO ()
main = TF.defaultMain tests

tests :: [TF.Test]
tests = [ TF.testGroup "Positive tests"
          [ testCase "compile HelloWorld.ll"
                     pCompileHelloWorldLL
          , testCase "compile HelloWorld.bc"
                     pCompileHelloWorldBC
          , testCase "compile multiple .ll files"
                     pCompileMultipleLL
          , testCase "compile multiple .bc files"
                     pCompileMultipleBC
          , testCase "compile multiple .ll and .bc files"
                     pCompileMultipleLLBC
          ]
        , TF.testGroup "Negative tests"
          [ testCase "compile HelloWorldWithError.ll"
                     nCompileHelloWorldWithErrorLL
          , testCase "compile multiple .ll files with redefinitions"
                     nCompileMultipleLL
          , testCase "compile multiple .bc files with redefinitions"
                     nCompileMultipleBC
          , testCase "compile multiple .ll and .bc files with redefinitions"
                     nCompileMultipleLLBC
          ]
        ]

pCompileHelloWorldLL :: Assertion
pCompileHelloWorldLL =
  B8.readFile "Test/data/HelloWorld.ll" >>= \ll ->
  compileModule ll [] >>= (Success @=?) . errorCode

pCompileHelloWorldBC :: Assertion
pCompileHelloWorldBC =
  B.readFile "Test/data/HelloWorld.bc" >>= \bc ->
  compileModule bc [] >>= (Success @=?) . errorCode

pCompileMultipleLL :: Assertion
pCompileMultipleLL =
  sequence [ B8.readFile "Test/data/HelloWorld0.ll"
           , B8.readFile "Test/data/HelloWorld1.ll"
           , B8.readFile "Test/data/HelloWorld2.ll"
           , B8.readFile "Test/data/HelloWorld3.ll"
           ] >>= flip compileModules [] >>= (Success @=?) . errorCode

pCompileMultipleBC :: Assertion
pCompileMultipleBC =
  sequence [ B.readFile "Test/data/HelloWorld0.ll"
           , B.readFile "Test/data/HelloWorld1.ll"
           , B.readFile "Test/data/HelloWorld2.ll"
           , B.readFile "Test/data/HelloWorld3.ll"
           ] >>= flip compileModules [] >>= (Success @=?) . errorCode

pCompileMultipleLLBC :: Assertion
pCompileMultipleLLBC =
  sequence [ B8.readFile "Test/data/HelloWorld0.ll"
           , B.readFile  "Test/data/HelloWorld1.bc"
           , B8.readFile "Test/data/HelloWorld2.ll"
           , B.readFile  "Test/data/HelloWorld3.bc"
           ] >>= flip compileModules [] >>= (Success @=?) . errorCode

nCompileHelloWorldWithErrorLL :: Assertion
nCompileHelloWorldWithErrorLL =
  B8.readFile "Test/data/HelloWorldWithError.ll" >>=
  flip compileModule [] >>= (ErrorCompilation @=?) . errorCode

nCompileMultipleLL :: Assertion
nCompileMultipleLL =
  sequence [ B8.readFile "Test/data/HelloWorld.ll"
           , B8.readFile "Test/data/HelloWorld.ll"
           ] >>= flip compileModules [] >>= (ErrorCompilation @=?) . errorCode

nCompileMultipleBC :: Assertion
nCompileMultipleBC =
  sequence [ B.readFile "Test/data/HelloWorld.bc"
           , B.readFile "Test/data/HelloWorld.bc"
           ] >>= flip compileModules [] >>= (ErrorCompilation @=?) . errorCode

nCompileMultipleLLBC :: Assertion
nCompileMultipleLLBC =
  sequence [ B8.readFile "Test/data/HelloWorld.ll"
           , B.readFile  "Test/data/HelloWorld.bc"
           ] >>= flip compileModules [] >>= (ErrorCompilation @=?) . errorCode
