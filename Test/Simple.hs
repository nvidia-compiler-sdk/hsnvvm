module Main (main) where

import Foreign.LibNVVM.Internal
import Foreign.LibNVVM
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test)

import Control.Exception as E (bracket, catch, try)
import Data.Version(Version(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Paths_libnvvm as P
import qualified Test.Framework as TF (Test, defaultMain, testGroup)

main :: IO ()
main = TF.defaultMain tests

tests :: [TF.Test]
tests = [ TF.testGroup "Positive tests"
          [ TF.testGroup "Low-level API"
            [ testCase "initialize and finalize"
                       plInitializeFinalize
            , testCase "get the version number"
                       plVersion
            , testCase "create and destroy a compilation unit"
                       plCreateDestroyCompilationUnit
            , testCase "compile HelloWorld.ll"
                       plCompileHelloWorldLL
            , testCase "compile HelloWorld.bc"
                       plCompileHelloWorldBC
            , testCase "compile multiple .ll files"
                       plCompileMultipleLL
            , testCase "compile multiple .bc files"
                       plCompileMultipleBC
            , testCase "compile multiple .ll and .bc files"
                       plCompileMultipleLLBC
            ]
          , TF.testGroup "High-level API"
            [ testCase "compile HelloWorld.ll"
                       phCompileHelloWorldLL
            , testCase "compile HelloWorld.bc"
                       phCompileHelloWorldBC
            , testCase "compile multiple .ll files"
                       phCompileMultipleLL
            , testCase "compile multiple .bc files"
                       phCompileMultipleBC
            , testCase "compile multiple .ll and .bc files"
                       phCompileMultipleLLBC
            ]
          ]
        , TF.testGroup "Negative tests"
          [ TF.testGroup "Low-level API"
            [ testCase "finalize without initializing"
                       nlFinalize
            , testCase "get the version number without initializing"
                       nlVersion
            , testCase "create and destroy a compilation unit without initializing"
                       nlCreateDestroyCompilationUnit
            --, testCase "destroy a compilation unit twice"
            --           nlDestroyCompilationUnitTwice
            , testCase "compile HelloWorldWithError.ll"
                       nlCompileHelloWorldWithErrorLL
            , testCase "compile multiple .ll files with redefinitions"
                       nlCompileMultipleLL
            , testCase "compile multiple .bc files with redefinitions"
                       nlCompileMultipleBC
            , testCase "compile multiple .ll and .bc files with redefinitions"
                       nlCompileMultipleLLBC
            ]
          , TF.testGroup "High-level API"
            [ testCase "compile HelloWorldWithError.ll"
                       nhCompileHelloWorldWithErrorLL
            , testCase "compile multiple .ll files with redefinitions"
                       nhCompileMultipleLL
            , testCase "compile multiple .bc files with redefinitions"
                       nhCompileMultipleBC
            , testCase "compile multiple .ll and .bc files with redefinitions"
                       nhCompileMultipleLLBC
            ]
          ]
        ]

plInitializeFinalize :: Assertion
plInitializeFinalize =
  E.catch (E.bracket initialize (const finalize) return)
          (\e -> False @? show (e :: LibNVVMException))

plVersion :: Assertion
plVersion =
  E.catch (E.bracket initialize (const finalize) $ \_ ->
           version >>= compareAndAssert P.version)
          (\e -> False @? show (e :: LibNVVMException))
  where
    compareAndAssert :: Version -> Version -> Assertion
    compareAndAssert x y =
      (take 2 $ versionBranch x) @=? (take 2 $ versionBranch y)

plCreateDestroyCompilationUnit :: Assertion
plCreateDestroyCompilationUnit =
  E.catch (E.bracket initialize (const finalize) $ \_ -> create >>= destroy)
          (\e -> False @? show (e :: LibNVVMException))

plCompileHelloWorldLL :: Assertion
plCompileHelloWorldLL =
  E.catch (E.bracket initialize (const finalize) $ \_ ->
           E.bracket create destroy $ \cu ->
           B8.readFile "Test/data/HelloWorld.ll" >>= \ll ->
           addModule cu ll >> compile cu [] >>= \result ->
           if result
           then getCompiledResult cu >> return ()
           else Success @=? ErrorCompilation)
          (\e -> False @? show (e :: LibNVVMException))

plCompileHelloWorldBC :: Assertion
plCompileHelloWorldBC =
  E.catch (E.bracket initialize (const finalize) $ \_ ->
           E.bracket create destroy $ \cu ->
           B.readFile "Test/data/HelloWorld.bc" >>= \bc ->
           addModule cu bc >> compile cu [] >>= \result ->
           if result
           then getCompiledResult cu >> return ()
           else Success @=? ErrorCompilation)
          (\e -> False @? show (e :: LibNVVMException))

plCompileMultipleLL :: Assertion
plCompileMultipleLL =
  E.catch (E.bracket initialize (const finalize) $ \_ ->
           E.bracket create destroy $ \cu ->
           B8.readFile "Test/data/HelloWorld0.ll" >>= addModule cu >>
           B8.readFile "Test/data/HelloWorld1.ll" >>= addModule cu >>
           B8.readFile "Test/data/HelloWorld2.ll" >>= addModule cu >>
           B8.readFile "Test/data/HelloWorld3.ll" >>= addModule cu >>
           compile cu [] >>= \result ->
           if result
           then getCompiledResult cu >> return ()
           else Success @=? ErrorCompilation)
          (\e -> False @? show (e :: LibNVVMException))

plCompileMultipleBC :: Assertion
plCompileMultipleBC =
  E.catch (E.bracket initialize (const finalize) $ \_ ->
           E.bracket create destroy $ \cu ->
           B.readFile "Test/data/HelloWorld0.bc" >>= addModule cu >>
           B.readFile "Test/data/HelloWorld1.bc" >>= addModule cu >>
           B.readFile "Test/data/HelloWorld2.bc" >>= addModule cu >>
           B.readFile "Test/data/HelloWorld3.bc" >>= addModule cu >>
           compile cu [] >>= \result ->
           if result
           then getCompiledResult cu >> return ()
           else Success @=? ErrorCompilation)
          (\e -> False @? show (e :: LibNVVMException))

plCompileMultipleLLBC :: Assertion
plCompileMultipleLLBC =
  E.catch (E.bracket initialize (const finalize) $ \_ ->
           E.bracket create destroy $ \cu ->
           B8.readFile "Test/data/HelloWorld0.ll" >>= addModule cu >>
           B.readFile  "Test/data/HelloWorld1.bc" >>= addModule cu >>
           B8.readFile "Test/data/HelloWorld2.ll" >>= addModule cu >>
           B.readFile  "Test/data/HelloWorld3.bc" >>= addModule cu >>
           compile cu [] >>= \result ->
           if result
           then getCompiledResult cu >> return ()
           else Success @=? ErrorCompilation)
          (\e -> False @? show (e :: LibNVVMException))

phCompileHelloWorldLL :: Assertion
phCompileHelloWorldLL =
  B8.readFile "Test/data/HelloWorld.ll" >>= \ll ->
  compileModule ll [] >>= (Success @=?) . errorCode

phCompileHelloWorldBC :: Assertion
phCompileHelloWorldBC =
  B.readFile "Test/data/HelloWorld.bc" >>= \bc ->
  compileModule bc [] >>= (Success @=?) . errorCode

phCompileMultipleLL :: Assertion
phCompileMultipleLL =
  sequence [ B8.readFile "Test/data/HelloWorld0.ll"
           , B8.readFile "Test/data/HelloWorld1.ll"
           , B8.readFile "Test/data/HelloWorld2.ll"
           , B8.readFile "Test/data/HelloWorld3.ll"
           ] >>= flip compileModules [] >>= (Success @=?) . errorCode

phCompileMultipleBC :: Assertion
phCompileMultipleBC =
  sequence [ B.readFile "Test/data/HelloWorld0.ll"
           , B.readFile "Test/data/HelloWorld1.ll"
           , B.readFile "Test/data/HelloWorld2.ll"
           , B.readFile "Test/data/HelloWorld3.ll"
           ] >>= flip compileModules [] >>= (Success @=?) . errorCode

phCompileMultipleLLBC :: Assertion
phCompileMultipleLLBC =
  sequence [ B8.readFile "Test/data/HelloWorld0.ll"
           , B.readFile  "Test/data/HelloWorld1.bc"
           , B8.readFile "Test/data/HelloWorld2.ll"
           , B.readFile  "Test/data/HelloWorld3.bc"
           ] >>= flip compileModules [] >>= (Success @=?) . errorCode

nlFinalize :: Assertion
nlFinalize =
  E.try finalize >>= \x -> case x of
    Right _                    -> ErrorNotInitialized @=? Success
    Left  (LibNVVMException e) -> ErrorNotInitialized @=? e

nlVersion :: Assertion
nlVersion =
  E.try version >>= \x -> case x of
    Right _                    -> ErrorNotInitialized @=? Success
    Left  (LibNVVMException e) -> ErrorNotInitialized @=? e

nlCreateDestroyCompilationUnit :: Assertion
nlCreateDestroyCompilationUnit =
  E.try (create >>= destroy) >>= \x -> case x of
    Right _                    -> ErrorNotInitialized @=? Success
    Left  (LibNVVMException e) -> ErrorNotInitialized @=? e

{-
nlDestroyCompilationUnitTwice :: Assertion
nlDestroyCompilationUnitTwice =
  E.try (E.bracket initialize (const finalize) $ \_ ->
         E.bracket create destroy destroy) >>= \x -> case x of
    Right _                    -> ErrorInvalidCu @=? Success
    Left  (LibNVVMException e) -> ErrorInvalidCu @=? e
-}

nlCompileHelloWorldWithErrorLL :: Assertion
nlCompileHelloWorldWithErrorLL =
  E.catch (E.bracket initialize (const finalize) $ \_ ->
           E.bracket create destroy $ \cu ->
           B8.readFile "Test/data/HelloWorldWithError.ll" >>= \ll ->
           addModule cu ll >> compile cu [] >>= \result ->
           if result then ErrorCompilation @=? Success else return ())
          (\e -> False @? show (e :: LibNVVMException))

nlCompileMultipleLL :: Assertion
nlCompileMultipleLL =
  E.catch (E.bracket initialize (const finalize) $ \_ ->
           E.bracket create destroy $ \cu ->
           B8.readFile "Test/data/HelloWorld.ll" >>= addModule cu >>
           B8.readFile "Test/data/HelloWorld.ll" >>= addModule cu >>
           compile cu [] >>= \result ->
           if result then Success @=? ErrorCompilation else return ())
          (\e -> False @? show (e :: LibNVVMException))

nlCompileMultipleBC :: Assertion
nlCompileMultipleBC =
  E.catch (E.bracket initialize (const finalize) $ \_ ->
           E.bracket create destroy $ \cu ->
           B.readFile "Test/data/HelloWorld.bc" >>= addModule cu >>
           B.readFile "Test/data/HelloWorld.bc" >>= addModule cu >>
           compile cu [] >>= \result ->
           if result then Success @=? ErrorCompilation else return ())
          (\e -> False @? show (e :: LibNVVMException))

nlCompileMultipleLLBC :: Assertion
nlCompileMultipleLLBC =
  E.catch (E.bracket initialize (const finalize) $ \_ ->
           E.bracket create destroy $ \cu ->
           B8.readFile "Test/data/HelloWorld.ll" >>= addModule cu >>
           B.readFile  "Test/data/HelloWorld.bc" >>= addModule cu >>
           compile cu [] >>= \result ->
           if result then Success @=? ErrorCompilation else return ())
          (\e -> False @? show (e :: LibNVVMException))

nhCompileHelloWorldWithErrorLL :: Assertion
nhCompileHelloWorldWithErrorLL =
  B8.readFile "Test/data/HelloWorldWithError.ll" >>=
  flip compileModule [] >>= (ErrorCompilation @=?) . errorCode

nhCompileMultipleLL :: Assertion
nhCompileMultipleLL =
  sequence [ B8.readFile "Test/data/HelloWorld.ll"
           , B8.readFile "Test/data/HelloWorld.ll"
           ] >>= flip compileModules [] >>= (ErrorCompilation @=?) . errorCode

nhCompileMultipleBC :: Assertion
nhCompileMultipleBC =
  sequence [ B.readFile "Test/data/HelloWorld.bc"
           , B.readFile "Test/data/HelloWorld.bc"
           ] >>= flip compileModules [] >>= (ErrorCompilation @=?) . errorCode

nhCompileMultipleLLBC :: Assertion
nhCompileMultipleLLBC =
  sequence [ B8.readFile "Test/data/HelloWorld.ll"
           , B.readFile  "Test/data/HelloWorld.bc"
           ] >>= flip compileModules [] >>= (ErrorCompilation @=?) . errorCode
