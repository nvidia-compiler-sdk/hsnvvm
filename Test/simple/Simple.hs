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

-- Positive tests
-- --------------

assertSuccess :: Result -> Assertion
assertSuccess (Result _ (Right _)) = return ()
assertSuccess (Result _ (Left e))  = assertFailure (show e)


pCompileHelloWorldLL :: Assertion
pCompileHelloWorldLL = do
  ll <- B8.readFile "Test/data/HelloWorld.ll"
  r  <- compileModule ll "compileLL" []
  assertSuccess r

pCompileHelloWorldBC :: Assertion
pCompileHelloWorldBC = do
  bc <- B.readFile "Test/data/HelloWorld.bc"
  assertSuccess =<< compileModule bc "compileBC" []

pCompileMultipleLL :: Assertion
pCompileMultipleLL = do
  lls <- mapM B8.readFile [ "Test/data/HelloWorld0.ll"
                          , "Test/data/HelloWorld1.ll"
                          , "Test/data/HelloWorld2.ll"
                          , "Test/data/HelloWorld3.ll"
                          ]
  r   <- compileModules (zip lls (repeat "compileMultipleLL")) []
  assertSuccess r

pCompileMultipleBC :: Assertion
pCompileMultipleBC = do
  bcs <- mapM B.readFile [ "Test/data/HelloWorld0.bc"
                         , "Test/data/HelloWorld1.bc"
                         , "Test/data/HelloWorld2.bc"
                         , "Test/data/HelloWorld3.bc"
                         ]
  r   <- compileModules (zip bcs (repeat "compileMultipleBC")) []
  assertSuccess r

pCompileMultipleLLBC :: Assertion
pCompileMultipleLLBC = do
  llbcs <- sequence [ B8.readFile "Test/data/HelloWorld0.ll"
                    , B.readFile  "Test/data/HelloWorld1.bc"
                    , B8.readFile "Test/data/HelloWorld2.ll"
                    , B.readFile  "Test/data/HelloWorld3.bc"
                    ]
  r     <- compileModules (zip llbcs (repeat "compileMultipleLLBC")) []
  assertSuccess r


-- Negative tests
-- --------------

assertError :: Result -> Assertion
assertError (Result _ (Left ErrorCompilation)) = return ()
assertError (Result _ (Left e))                = ErrorCompilation @=? e
assertError (Result _ (Right _))               = ErrorCompilation @=? Success

nCompileHelloWorldWithErrorLL :: Assertion
nCompileHelloWorldWithErrorLL = do
  ll <- B8.readFile "Test/data/HelloWorldWithError.ll"
  r  <- compileModule ll "errorLL" []
  assertError r

nCompileMultipleLL :: Assertion
nCompileMultipleLL = do
  lls <- mapM B8.readFile [ "Test/data/HelloWorld.ll"
                          , "Test/data/HelloWorld.ll"
                          ]
  r   <- compileModules (zip lls (repeat "errorMultipleLL")) []
  assertError r

nCompileMultipleBC :: Assertion
nCompileMultipleBC = do
  bcs <- mapM B.readFile [ "Test/data/HelloWorld.bc"
                         , "Test/data/HelloWorld.bc"
                         ]
  r   <- compileModules (zip bcs (repeat "errorMultipleBC")) [] 
  assertError r

nCompileMultipleLLBC :: Assertion
nCompileMultipleLLBC = do
  llbcs <- sequence [ B8.readFile "Test/data/HelloWorld.ll"
                    , B.readFile  "Test/data/HelloWorld.bc"
                    ]
  r     <- compileModules (zip llbcs (repeat "errorMultipleLLBC")) []
  assertError r

