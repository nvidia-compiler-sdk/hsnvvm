-- |
-- Module      : Foreign.LibNVVM.Info
-- Copyright   : 2012 Sean Lee
--               2014 Trevor L. McDonell
-- License     :
--
-- Maintainer  : Sean Lee <selee@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.LibNVVM.Info (

  nvvmVersion,

) where

import Foreign.LibNVVM.Internal
import Foreign.LibNVVM.Error

import Data.Version
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal

#include <nvvm.h>
{# context lib="nvvm" #}


-- | Return the libNVVM version number
--
nvvmVersion :: IO Version
nvvmVersion = do
  (r, major,minor) <- nvvmVersion'
  resultIfOk (r, Version [major,minor] [])


{-# INLINE nvvmVersion' #-}
{# fun unsafe nvvmVersion as nvvmVersion'
  { alloca- `Int' peekIntConv*
  , alloca- `Int' peekIntConv*
  }
  -> `Status' cToEnum #}

