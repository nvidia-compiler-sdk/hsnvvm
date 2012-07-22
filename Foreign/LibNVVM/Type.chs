{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.LibNVVM.Type
-- Copyright   : 2012 Sean Lee
-- License     :
--
-- Maintainer  : Sean Lee <selee@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Foreign.LibNVVM.Type (
  -- * libNVVM Compilation Unit
  CompilationUnit
) where

#include <nvvm.h>

import Foreign.Ptr (Ptr)

{# context lib = "nvvm" #}

data NVVMCompilationUnit
{# pointer nvvmCU as CompilationUnit -> NVVMCompilationUnit #}
