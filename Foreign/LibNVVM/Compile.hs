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

import qualified Control.Exception as E (bracket, bracket_, catch, try)

data Result = Result {errorCode      :: ErrorCode,
                      compilationLog :: ByteString,
                      ptx            :: ByteString}

compileModule :: ByteString -> [String] -> IO Result
compileModule m = compileModules [m]

compileModules :: [ByteString] -> [String] -> IO Result
compileModules ms options =
  E.catch (E.bracket_ initialize finalize $
           E.bracket  create     destroy  $ \cu ->
           E.try (mapM_ (addModule cu) ms >>
                  compile cu options >>
                  getCompiledResult cu) >>= \p ->
           getCompilationLog cu >>= \compLog -> return $ case p of
           Right p'                   -> Result Success compLog p'
           Left  (LibNVVMException e) -> Result e compLog empty)
          (\(LibNVVMException e) -> return $ Result e empty empty)
