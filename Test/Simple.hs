module Main where

import Control.Exception (bracket, catch, try)
import Foreign.LibNVVM
import Prelude hiding (catch)
import qualified Test.Framework as TF (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

pInitializeFinalize :: Assertion
pInitializeFinalize =
  catch (bracket initialize (const finalize) return)
        (\e -> False @? show (e :: LibNVVMException))

pVersion :: Assertion
pVersion =
  catch (bracket initialize (const finalize) $ \_ -> version >> return ())
        (\e -> False @? show (e :: LibNVVMException))

pCreateDestroyCompilationUnit :: Assertion
pCreateDestroyCompilationUnit =
  catch (bracket initialize (const finalize) $ \_ -> create >>= destroy)
        (\e -> False @? show (e :: LibNVVMException))

nFinalize :: Assertion
nFinalize =
  try finalize >>= \x -> case x of
    Right _                    -> ErrorNotInitialized @=? Success
    Left  (LibNVVMException e) -> ErrorNotInitialized @=? e

nVersion :: Assertion
nVersion =
  try version >>= \x -> case x of
    Right _                    -> ErrorNotInitialized @=? Success
    Left  (LibNVVMException e) -> ErrorNotInitialized @=? e

nCreateDestroyCompilationUnit :: Assertion
nCreateDestroyCompilationUnit =
  try (create >>= destroy) >>= \x -> case x of
    Right _                    -> ErrorNotInitialized @=? Success
    Left  (LibNVVMException e) -> ErrorNotInitialized @=? e

nDestroyCompilationUnitTwice :: Assertion
nDestroyCompilationUnitTwice =
  try (bracket initialize (const finalize) $ \_ ->
         bracket create destroy destroy) >>= \x -> case x of
    Right _                    -> ErrorInvalidCu @=? Success
    Left  (LibNVVMException e) -> ErrorInvalidCu @=? e

main :: IO ()
main = TF.defaultMain tests

tests :: [TF.Test]
tests = [ TF.testGroup "Positive tests"
          [ testCase "initialize and finalize"
                     pInitializeFinalize
          , testCase "get the version number"
                     pVersion
          , testCase "create and destroy a compilation unit"
                     pCreateDestroyCompilationUnit
          ]
        , TF.testGroup "Negative tests"
          [ testCase "finalize without initializing"
                     nFinalize
          , testCase "get the version number without initializing"
                     pVersion
          , testCase "create and destroy a compilation unit without initializing"
                     nCreateDestroyCompilationUnit
          --, testCase "destroy a compilation unit twice"
          --           nDestroyCompilationUnitTwice
          ]
        ]
