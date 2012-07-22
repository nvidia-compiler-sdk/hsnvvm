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
  initialize, finalize, version, create, destroy, addModule, compile,
  getCompiledResult, getCompilationLog
) where

#include <nvvm.h>

import Control.Applicative (pure, (<$>))
import Foreign.C.String (newCString, peekCString, withCStringLen)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (allocaArray, withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

{# import Foreign.LibNVVM.Error #}
{# import Foreign.LibNVVM.Type #}
{# context lib = "nvvm" #}

initialize :: IO ()
initialize = toEC <$> {# call unsafe nvvmInit #} >>= flip checkError ()

finalize :: IO ()
finalize = toEC <$> {# call unsafe nvvmFini #} >>= flip checkError ()

version :: IO (Int, Int)
version = alloca $ \major -> alloca $ \minor ->
  toEC <$> {# call unsafe nvvmVersion #} major minor >>= \status ->
  fromIntegral <$> peek major >>= \major' ->
  fromIntegral <$> peek minor >>= \minor' ->
  checkError status (major', minor')

create :: IO CompilationUnit
create = alloca $ \cuptr ->
  toEC <$> {# call unsafe nvvmCreateCU #} cuptr >>= \status ->
  peek cuptr >>= checkError status

destroy :: CompilationUnit -> IO ()
destroy cu = with cu $ \cuptr ->
  toEC <$> {# call unsafe nvvmDestroyCU #} cuptr >>= flip checkError ()

addModule :: CompilationUnit -> String -> IO ()
addModule cu m = withCStringLen m $ \(m', size) ->
  fromIntegral <$> pure size >>= \size' ->
  toEC <$> {# call unsafe nvvmCUAddModule #} cu m' size' >>= \status ->
  checkError status ()

compile :: CompilationUnit -> [String] -> IO Bool
compile cu opts = mapM newCString opts >>= \opts' ->
  withArray opts' $ \opts'' ->
  toEC <$> {# call unsafe nvvmCompileCU #} cu numOpts opts'' >>= \status ->
  mapM_ free opts' >> checkCompileError status
  where
    numOpts :: CInt
    numOpts = fromIntegral $ length opts

getCompiledResult :: CompilationUnit -> IO String
getCompiledResult cu = getCompiledResultSize >>= \size ->
  allocaArray size $ \result ->
  toEC <$> {# call unsafe nvvmGetCompiledResult #} cu result >>= \status ->
  peekCString result >>= checkError status
  where
    getCompiledResultSize :: IO Int
    getCompiledResultSize = alloca $ \size ->
      toEC <$> {# call unsafe nvvmGetCompiledResultSize #} cu size >>= \status ->
      fromIntegral <$> peek size >>= checkError status

getCompilationLog :: CompilationUnit -> IO String
getCompilationLog cu = getCompilationLogSize >>= \size ->
  allocaArray size $ \result ->
  toEC <$> {# call unsafe nvvmGetCompilationLog #} cu result >>= \status ->
  peekCString result >>= checkError status
  where
    getCompilationLogSize :: IO Int
    getCompilationLogSize = alloca $ \size ->
      toEC <$> {# call unsafe nvvmGetCompilationLogSize #} cu size >>= \status ->
      fromIntegral <$> peek size >>= checkError status