{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable       #-}
-- |
-- Module      : Foreign.LibNVVM.Internal
-- Copyright   : 2012 Sean Lee
-- License     :
--
-- Maintainer  : Sean Lee <selee@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides (almost) one-to-one mapping between libNVVM and
-- Haskell.
module Foreign.LibNVVM.Internal (

  -- * libNVVM Program
  Program,

  -- * Query the libNVVM version number
  version,

  -- * Create, destroy, and manipulate compilation unit
  create, destroy, addModule,

  -- * Compile
  compile, getCompiledResult, getProgramLog,

  -- * libNVVM exception
  LibNVVMException(..), checkError

) where

#include <nvvm.h>

import Control.Applicative ((<$>))
import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.ByteString.Char8 (ByteString, packCString, useAsCStringLen)
import Data.Typeable (Typeable)
import Data.Version (Version(..))
import Foreign.C.String (newCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (allocaArray, withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

{# import Foreign.LibNVVM.Error #}
{# context lib = "nvvm" #}
{# pointer nvvmProgram as Program #}


-- |
-- Returns the libNVVM version number.
--
version :: IO Version
version = alloca $ \major -> alloca $ \minor ->
  toErrorCode <$> {# call unsafe nvvmVersion #} major minor >>= \status ->
  fromIntegral <$> peek major >>= \major' ->
  fromIntegral <$> peek minor >>= \minor' ->
  checkError status $ Version [major', minor'] []

-- |
-- 'create' creates a program compilation unit, which is a handle used by
-- 'destroy', 'addModule', 'compile', 'getCompiledResult', and 'getProgramLog'.
--
create :: IO Program
create = alloca $ \cuptr ->
  toErrorCode <$> {# call unsafe nvvmCreateProgram #} cuptr >>= \status ->
  peek cuptr >>= checkError status

-- |
-- 'destroy' destroys the given program compilation unit.
--
-- It raises a 'LibNVVMException', if it is passed an invalid program
-- compilation unit.
--
destroy :: Program -> IO ()
destroy cu = with cu $ \cuptr ->
  toErrorCode <$> {# call unsafe nvvmDestroyProgram #} cuptr >>= flip checkError ()

-- |
-- 'addModule' adds a module level NVVM IR to the given program compilation unit.
--
-- It raises a 'LibNVVMException', if it is:
--
-- * passed an invalid compilation unit; or
--
-- * passed an invalid NVVM module IR.
--
addModule :: Program
          -> ByteString      -- ^ an NVVM module IR either in the bitcode
                             --   representation or in the text
                             --   representation.
          -> String          -- ^ name of the NVVM IR module
          -> IO ()
addModule prog m name = do
  name' <- if null name
              then return nullPtr       -- defaults to "<unnamed>"
              else newCString name
  --
  useAsCStringLen m $ \(m', size) -> do
    res <- {# call unsafe nvvmAddModuleToProgram #} prog m' (fromIntegral size) name'
    unless (null name) (free name')
    checkError (toErrorCode res) ()

-- |
-- 'compile' compiles the NVVM IR modules that have been added to the
-- compilation unit.
--
-- The valid compiler options are
--
-- [@-target=\<value\>@]
--   \<value\>: ptx (default), verify
--
-- [@-g@]
--
-- [@-opt=\<level\>@]
--   \<level\>: 0, 3 (default)
--
-- [@-arch=\<arch_value\>@]
--   \<arch_value\>: compute_20 (default), compute_30
--
-- [@-ftz=\<value\>@]
--   \<value\>: 0 (default, preserve denormal values, when performing
--                 single-precision floating-point operations),
--              1 (flush denormal values to zero, when performing
--                 single-precision floating-point operations)
--
-- [@-prec-sqrt=\<value\>@]
--   \<value\>: 0 (use a faster approximation for single-precision
--                 floating-point square root),
--              1 (default, use IEEE round-to-nearest mode for
--                 single-precision floating-point square root)
--
-- [@-prec-div=\<value\>@]
--   \<value\>: 0 (use a faster approximation for single-precision
--                 floating-point division and reciprocals),
--              1 (default, use IEEE round-to-nearest mode for
--                 single-precision floating-point division and reciprocals)
--
-- [@-fma=\<value\>@]
--   \<value\>: 0 (disable FMA contraction),
--              1 (default, enable FMA contraction)
--
-- It raises a 'LibNVVMException', if it is
--
-- * called before 'initialize',
--
-- * passed an invalid compilation unit,
--
-- * passed a compilation unit with an NVVM IR module with an incompatible IR
--   version,
--
-- * passed a compilation unit without any NVVM IR module,
--
-- * passed a compilation unit with a NVVM IR module that has an error, or
--
-- * passed an unrecognized compiler option.
--
-- It also produces the PTX output that can be retrieved, using
-- 'getCompiledResult' and the compilation log that can retrieved, using
-- 'getProgramLog'.
--
compile :: Program
        -> [String]        -- ^ compiler options
        -> IO ()
compile cu opts = mapM newCString opts >>= \opts' ->
  withArray opts' $ \opts'' ->
  toErrorCode <$> {# call unsafe nvvmCompileProgram #} cu numOpts opts'' >>= \status ->
  mapM_ free opts' >>= checkError status
  where
    numOpts :: CInt
    numOpts = fromIntegral $ length opts

-- |
-- 'getCompiledResult' returns the 'ByteString' that contains the PTX output
-- generated by 'compile'.
--
-- It raises a 'LibNVVMException', if it is
--
-- * called before 'initialize', or
--
-- * passed an invalid compilation unit,
--
getCompiledResult :: Program
                  -> IO ByteString   -- ^ PTX output generated by 'compile'
getCompiledResult cu = getCompiledResultSize >>= \size ->
  allocaArray size $ \result ->
  toErrorCode <$> {# call unsafe nvvmGetCompiledResult #} cu result >>= \status ->
  packCString result >>= checkError status
  where
    getCompiledResultSize :: IO Int
    getCompiledResultSize = alloca $ \size ->
      toErrorCode <$> {# call unsafe nvvmGetCompiledResultSize #} cu size >>= \status ->
      fromIntegral <$> peek size >>= checkError status

-- |
-- 'getProgramLog' returns the 'ByteString' that contains the compilation
-- log generated by 'compile'.
--
-- It raises a 'LibNVVMException', if it is
--
-- * called before 'initialize', or
--
-- * passed an invalid compilation unit,
--
getProgramLog :: Program -> IO ByteString
getProgramLog cu = getProgramLogSize >>= \size ->
  allocaArray size $ \result ->
  toErrorCode <$> {# call unsafe nvvmGetProgramLog #} cu result >>= \status ->
  packCString result >>= checkError status
  where
    getProgramLogSize :: IO Int
    getProgramLogSize = alloca $ \size ->
      toErrorCode <$> {# call unsafe nvvmGetProgramLogSize #} cu size >>= \status ->
      fromIntegral <$> peek size >>= checkError status

-- |
-- 'LibNVVMException' lifts 'ErrorCode' into an exception.
--
data LibNVVMException = LibNVVMException ErrorCode
                        deriving (Typeable, Show)

instance Exception LibNVVMException

-- |
-- Raise a 'LibNVVMException' if there is an error of any kind. Otherwise,
-- return the value.
--
checkError :: ErrorCode -> a -> IO a
checkError Success a = return a
checkError status  _ = throwIO $ LibNVVMException status

