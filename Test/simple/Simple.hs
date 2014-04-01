-- Copyright (c) 2012-2014 NVIDIA Corporation.  All rights reserved.
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Main (main) where

import Foreign.LibNVVM

import Control.Monad
import Control.Exception
import Text.Printf
import Test.HUnit                                       hiding (Test)
import Test.Framework.Providers.HUnit                   (testCase)

import qualified Data.ByteString                        as B
import qualified Data.ByteString.Char8                  as B8
import qualified Test.Framework                         as TF (Test, defaultMain, testGroup)

main :: IO ()
main = TF.defaultMain tests

tests :: [TF.Test]
tests =
  [ TF.testGroup "Positive tests"
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


assertRaises
    :: (Exception e, Show e, Eq e)
    => String
    -> e
    -> IO a
    -> Assertion
assertRaises msg selector action =
  let testErr e
        | e == selector = return ()
        | otherwise     = assertFailure $ printf "%s\nReceived unexpected exception: %s\ninstead of exception: %s"
                                                 msg (show e) (show selector)
  in do
    r <- try action
    case r of
      Left e  -> testErr e
      Right _ -> assertFailure $ printf "%s\nReceived no exception, but was expecting: %s" msg (show selector)


-- Compilation actually returns a UserError so that the compilation error
-- message is returned as part of the string. The first line of the error
-- message does contain the Status error string, however, so just compare that.
--
instance Eq NVVMException where
  s == t = head (lines (show s)) == head (lines (show t))


-- Positive tests
-- --------------

pCompileHelloWorldLL :: Assertion
pCompileHelloWorldLL = do
  ll   <- B8.readFile "Test/data/HelloWorld.ll"
  void $! compileModule "compileLL" ll [] True

pCompileHelloWorldBC :: Assertion
pCompileHelloWorldBC = do
  bc   <- B.readFile "Test/data/HelloWorld.bc"
  void $! compileModule "compileBC" bc [] True

pCompileMultipleLL :: Assertion
pCompileMultipleLL = do
  lls <- mapM B8.readFile [ "Test/data/HelloWorld0.ll"
                          , "Test/data/HelloWorld1.ll"
                          , "Test/data/HelloWorld2.ll"
                          , "Test/data/HelloWorld3.ll"
                          ]
  void $! compileModules (zip (repeat "compileMultipleLL") lls) [] True

pCompileMultipleBC :: Assertion
pCompileMultipleBC = do
  bcs <- mapM B.readFile [ "Test/data/HelloWorld0.bc"
                         , "Test/data/HelloWorld1.bc"
                         , "Test/data/HelloWorld2.bc"
                         , "Test/data/HelloWorld3.bc"
                         ]
  void $! compileModules (zip (repeat "compileMultipleBC") bcs) [] True

pCompileMultipleLLBC :: Assertion
pCompileMultipleLLBC = do
  llbcs <- sequence [ B8.readFile "Test/data/HelloWorld0.ll"
                    , B.readFile  "Test/data/HelloWorld1.bc"
                    , B8.readFile "Test/data/HelloWorld2.ll"
                    , B.readFile  "Test/data/HelloWorld3.bc"
                    ]
  void $! compileModules (zip (repeat "compileMultipleLLBC") llbcs) [] True

-- Negative tests
-- --------------

compilationError :: IO Result -> Assertion
compilationError action =
  assertRaises "Compilation Error" (ExitCode Compilation) action


nCompileHelloWorldWithErrorLL :: Assertion
nCompileHelloWorldWithErrorLL = do
  ll <- B8.readFile "Test/data/HelloWorldWithError.ll"
  compilationError $ compileModule "errorLL" ll [] True

nCompileMultipleLL :: Assertion
nCompileMultipleLL = do
  lls <- mapM B8.readFile [ "Test/data/HelloWorld.ll"
                          , "Test/data/HelloWorld.ll"
                          ]
  compilationError $ compileModules (zip (repeat "errorMultipleLL") lls) [] True

nCompileMultipleBC :: Assertion
nCompileMultipleBC = do
  bcs <- mapM B.readFile [ "Test/data/HelloWorld.bc"
                         , "Test/data/HelloWorld.bc"
                         ]
  compilationError $ compileModules (zip (repeat "errorMultipleBC") bcs) [] True

nCompileMultipleLLBC :: Assertion
nCompileMultipleLLBC = do
  llbcs <- sequence [ B8.readFile "Test/data/HelloWorld.ll"
                    ,  B.readFile "Test/data/HelloWorld.bc"
                    ]
  compilationError $ compileModules (zip (repeat "errorMultipleLLBC") llbcs) [] True

