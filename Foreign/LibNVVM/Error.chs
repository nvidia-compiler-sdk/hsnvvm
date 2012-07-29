{-# LANGUAGE ForeignFunctionInterface #-}
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
  -- * Error code
  ErrorCode(..), toErrorCode,
) where

#include <nvvm.h>

{# context lib = "nvvm" #}

-- |
-- The type 'ErrorCode' is an enumeration whose values represent the status of
-- the last libNVVM operation.
--
{# enum nvvmResult as ErrorCode {underscoreToCase}
                   with prefix="NVVM" deriving (Eq, Show) #}

-- |
-- Coercion from an integral type to 'ErrorCode'
--
toErrorCode :: (Integral a) => a -> ErrorCode
toErrorCode = toEnum . fromIntegral
