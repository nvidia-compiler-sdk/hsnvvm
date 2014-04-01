{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
-- |
-- Module      : Foreign.LibNVVM.Error
-- Copyright   : NVIDIA Corporation
-- License     : MIT
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
  showsPrec _ (UserError s) = showString s

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

