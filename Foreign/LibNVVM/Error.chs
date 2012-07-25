{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable       #-}
-- |
-- Module      : Foreign.LibNVVM.Error
-- Copyright   : 2012 Sean Lee
-- License     :
--
-- Maintainer  : Sean Lee <selee@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Foreign.LibNVVM.Error (
  LibNVVMException(..), ErrorCode(..),
  toErrorCode, checkError, checkCompileError
) where

#include <nvvm.h>

import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)

{# context lib = "nvvm" #}

--
-- Exception
--
data LibNVVMException = LibNVVMException ErrorCode
                        deriving (Typeable)

instance Exception LibNVVMException

instance Show LibNVVMException where
  show (LibNVVMException status) = "libNVVM Exception: " ++ show status

--
-- Error Code
--
{# enum nvvmResult as ErrorCode {underscoreToCase}
                   with prefix="NVVM" deriving (Eq, Show) #}

-- |
-- Coercion from integral types to 'ErrorCode'
--
toErrorCode :: (Integral a) => a -> ErrorCode
toErrorCode = toEnum . fromIntegral

-- |
-- Raise an exception if there is an error of any kind. Otherwise, return the
-- value.
--
checkError :: ErrorCode -> a -> IO a
checkError Success a = return a
checkError status  _ = throwIO $ LibNVVMException status

-- |
-- Return True on successful compilation. Return False if there is a
-- compilation error, and raise an exception for other kinds of errors.
--
checkCompileError :: ErrorCode -> IO Bool
checkCompileError Success          = return True
checkCompileError ErrorCompilation = return False
checkCompileError status           = throwIO $ LibNVVMException status
