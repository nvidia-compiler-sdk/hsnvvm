{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable       #-}
-- Copyright (c) 2012-2014 NVIDIA Corporation
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
-- Module      : Foreign.LibNVVM.Internal
-- Copyright   : NVIDIA Corporation
-- License     : MIT
--
-- Maintainer  : Sean Lee <selee@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.LibNVVM.Internal (

  withIntConv, withFloatConv, peekIntConv, peekFloatConv,

  cIntConv, cFloatConv,
  cToEnum, cFromEnum,

) where

import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable


-- Composite marshalling functions
-- -------------------------------

withIntConv :: (Storable b, Integral a, Integral b) => a -> (Ptr b -> IO c) -> IO c
withIntConv = with . cIntConv

withFloatConv :: (Storable b, RealFloat a, RealFloat b) => a -> (Ptr b -> IO c) -> IO c
withFloatConv = with . cFloatConv

peekIntConv :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
peekIntConv = liftM cIntConv . peek

peekFloatConv :: (Storable a, RealFloat a, RealFloat b) => Ptr a -> IO b
peekFloatConv = liftM cFloatConv . peek


-- Conversion routines
-- -------------------

{-# INLINE cIntConv #-}
cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral

{-# INLINE [1] cFloatConv #-}
cFloatConv :: (RealFloat a, RealFloat b) => a -> b
cFloatConv  = realToFrac
-- As this conversion by default goes via `Rational', it can be very slow...
{-# RULES
  "cFloatConv/Float->Float"    forall (x::Float).  cFloatConv x = x;
  "cFloatConv/Double->Double"  forall (x::Double). cFloatConv x = x;
  "cFloatConv/Float->CFloat"   forall (x::Float).  cFloatConv x = CFloat x;
  "cFloatConv/CFloat->Float"   forall (x::Float).  cFloatConv CFloat x = x;
  "cFloatConv/Double->CDouble" forall (x::Double). cFloatConv x = CDouble x;
  "cFloatConv/CDouble->Double" forall (x::Double). cFloatConv CDouble x = x
 #-}

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . cIntConv

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = cIntConv . fromEnum

