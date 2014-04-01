{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.LibNVVM.Error
-- Copyright   : 2012 Sean Lee
--               2014 Trevor L. McDonell
-- License     :
--
-- Maintainer  : Sean Lee <selee@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Foreign.LibNVVM.Error (

  -- * Error code
  Status(..), NVVMException(..),
  nvvmError, describe,

  -- * Helper functions
  resultIfOk, nothingIfOk,

) where

import Control.Exception
import Data.Typeable
import System.IO.Unsafe
import Foreign.C
import Foreign.Ptr
import Foreign.LibNVVM.Internal

#include "cbits/stubs.h"
{# context lib = "nvvm" #}

-- |
-- The type 'Status' is an enumeration whose values represent the status of the
-- last libNVVM operation.
--
{# enum nvvmResult as Status
    { underscoreToCase
    , NVVM_SUCCESS as Success
    }
    with prefix="NVVM_ERROR" deriving (Eq, Show) #}


-- | An NVVM Exception
--
data NVVMException
  = ExitCode  Status
  | UserError String
  deriving Typeable

instance Exception NVVMException

instance Show NVVMException where
  showsPrec n (ExitCode s)  = showsPrec n ("NVVM Exception: " ++ describe s)
  showsPrec n (UserError s) = showsPrec n s

-- | Raise an 'NVVMException' in the IO monad
--
nvvmError :: String -> IO a
nvvmError s = throwIO (UserError s)


-- Helper function
-- ---------------

-- | Get the message string for a given NVVM result code
--
{# fun pure unsafe nvvmGetErrorString as describe
    { cFromEnum `Status' } -> `String' #}

-- |
-- Return the results of a function on successful execution, otherwise throw an
-- exception with an error string associated with the return code
--
{-# INLINE resultIfOk #-}
resultIfOk :: (Status, a) -> IO a
resultIfOk (status, result) =
    case status of
        Success -> return  result
        _       -> throwIO (ExitCode status)

-- |
-- Throw an exception with an error string associated with an unsuccessful
-- return code, otherwise return unit.
--
{-# INLINE nothingIfOk #-}
nothingIfOk :: Status -> IO ()
nothingIfOk status =
    case status of
        Success -> return  ()
        _       -> throwIO (ExitCode status)

