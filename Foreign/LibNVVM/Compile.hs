{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.LibNVVM.Compile
-- Copyright   : 2012 Sean Lee
-- License     :
--
-- Maintainer  : Sean Lee <selee@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Foreign.LibNVVM.Compile (
  Result (..), compileModule, compileModules 
) where

import Data.ByteString
import Foreign.LibNVVM.Error
import Foreign.LibNVVM.Internal

import qualified Control.Exception as E (bracket, catch)

data Result = Result {errorCode      :: ErrorCode,
                      compilationLog :: ByteString,
                      ptx            :: ByteString}

compileModule :: ByteString -> [String] -> IO Result
compileModule m = compileModules [m]

compileModules :: [ByteString] -> [String] -> IO Result
compileModules ms options =
  E.catch (E.bracket initialize (const finalize) $ \_ ->
           E.bracket create     destroy $ \cu ->
           mapM_ (addModule cu) ms >> compile cu options >>= \status ->
           getCompilationLog cu >>= \compLog -> if status
           then getCompiledResult cu >>= return . Result Success compLog
           else return $ Result ErrorCompilation compLog empty)
          (\(LibNVVMException e) -> return $ Result e empty empty)
