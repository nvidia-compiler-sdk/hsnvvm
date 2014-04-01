{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
-- |
-- Module      : Foreign.LibNVVM.Compile
-- Copyright   : 2012 Sean Lee
--               2014 Trevor L. McDonell
-- License     :
--
-- Maintainer  : Sean Lee <selee@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Foreign.LibNVVM.Compile (

  -- * Compilation result
  Result(..), CompileFlag(..), VerifyFlag,

  -- * Compilation
  compileModule, compileModules,

) where

import Prelude                                          hiding ( log )
import Data.ByteString                                  ( ByteString )
import Data.Word
import Control.Exception
import Control.Monad
import Text.Printf
import qualified Data.ByteString.Char8                  as B
import qualified Data.ByteString.Unsafe                 as B
import qualified Data.ByteString.Internal               as B

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.CUDA.Analysis
import Foreign.LibNVVM.Error
import Foreign.LibNVVM.Internal

#include "cbits/stubs.h"
{# context lib="nvvm" #}


-- | The return type of compiling a module(s) is the compilation log together
-- with the resulting binary ptx/cubin.
--
data Result = Result {
    -- | Any compilation or verification messages and warnings that were
    -- generated during compilation of the NVVM module. Note that even upon
    -- successful completion, the log may not be empty.
    --
    nvvmLog       :: {-# UNPACK #-} !ByteString
  , nvvmResult    :: {-# UNPACK #-} !ByteString
  }


-- | An opaque handle to an NVVM program
--
newtype Program = Program { useProgram :: {# type nvvmProgram #}}
  deriving (Eq, Show)


-- | The available program compilation flags
--
data CompileFlag
    -- | Level of optimisation to apply (0-3) (Default: 3)
  = OptimisationLevel !Int

    -- | Compute architecture to target (Default: Compute 2.0)
  | Target !Compute

    -- | Flush denormal values to zero when performing single-precision floating
    -- point operations (Default: preserve denormal values)
  | FlushToZero

    -- | Use a faster approximation for single-precision floating-point square
    -- root (Default: use IEEE round-to-nearest mode)
  | FastSqrt

    -- | Use a faster approximation for single-precision floating-point division
    -- and reciprocal operations (Default: use IEEE round-to-nearest mode)
  | FastDiv

    -- | Disable fused-multiply-add contraction (Default: enabled)
  | DisableFMA

    -- | Generate debugging symbols (-g) (Default: no)
  | GenerateDebugInfo


-- | The available program verification flags
--
data VerifyFlag


compileFlagToArg :: CompileFlag -> String
compileFlagToArg f =
  case f of
    OptimisationLevel o  -> printf "-opt=%d" o
    Target (Compute n m) -> printf "-arch=compute_%d%d" n m
    FlushToZero          -> "-ftz=1"
    FastSqrt             -> "-prec-sqrt=0"
    FastDiv              -> "-prec-div=0"
    DisableFMA           -> "-fma=0"
    GenerateDebugInfo    -> "-g"


verifyFlagToArg :: VerifyFlag -> String
verifyFlagToArg _ = error "verifyFlagToArg"


-- High-level interface
-- --------------------

-- | Compile an NVVM IR module according to the specified options. If an error
-- occurs an exception is thrown, otherwise the generated PTX is returned
-- together with any warning or verification messages generated during
-- compilation.
--
-- The input NVVM IR module can be either in the bitcode representation or the
-- text representation.
--
compileModule
    :: String                           -- ^ name of the module (optional)
    -> ByteString                       -- ^ module NVVM IR
    -> [CompileFlag]                    -- ^ compilation options
    -> Bool                             -- ^ verify program?
    -> IO Result
compileModule name bc = compileModules [(name,bc)]


-- | Compile and link multiple NVVM IR modules together to form a single
-- program. The compiled result is represented in PTX.
--
-- Each NVVM IR module may individually be in either the bitcode representation
-- or in the text representation.
--
compileModules
    :: [(String, ByteString)]           -- ^ modules to compile and link
    -> [CompileFlag]                    -- ^ compilation options
    -> Bool                             -- ^ verify program?
    -> IO Result
compileModules modules opts verify =
  bracket create destroy $ \prg -> do
    res <- try $ do
      mapM_ (uncurry (addModule prg)) modules
      when verify (verifyProgram prg [])
      compileProgram prg opts
      compilerResult prg
    log   <- compilerLog prg
    case res of
      Left (err :: NVVMException) -> nvvmError (unlines [show err, B.unpack log])
      Right ptx                   -> return $! Result log ptx


-- The raw interface to libNVVM
-- ----------------------------

-- | Create a new program handle
--
create :: IO Program
create = resultIfOk =<< nvvmCreateProgram

{-# INLINE nvvmCreateProgram #-}
{# fun unsafe nvvmCreateProgram
  { alloca- `Program' peekProgram*
  }
  -> `Status' cToEnum #}
  where
    peekProgram = liftM Program . peek


-- | Destroy a program handle
--
destroy :: Program -> IO ()
destroy p = nothingIfOk =<< nvvmDestroyProgram p

{-# INLINE nvvmDestroyProgram #-}
{# fun unsafe nvvmDestroyProgram
  { withProgram* `Program' } -> `Status' cToEnum #}
  where
    withProgram = with . useProgram


-- | Add an NVVM IR module to the given program compilation unit. An exception
-- is raised if:
--
--   * The 'Program' to add to is invalid; or
--
--   * The NVVM module IR is invalid.
--
addModule :: Program -> String -> ByteString -> IO ()
addModule prg name mdl =
  B.unsafeUseAsCStringLen mdl $ \(b,n) -> do
    nothingIfOk =<< nvvmAddModuleToProgram prg b n name

{-# INLINE nvvmAddModuleToProgram #-}
{# fun unsafe nvvmAddModuleToProgram
  { useProgram    `Program'
  , id            `CString'
  , cIntConv      `Int'
  , withCString'* `String'
  }
  -> `Status' cToEnum #}
  where
    withCString' [] f   = f nullPtr             -- defaults to "<unnamed>"
    withCString' s  f   = withCString s f


-- | Compile the given 'Program' and all modules that have been previously added
-- to it.
--
compileProgram :: Program -> [CompileFlag] -> IO ()
compileProgram prg opts =
  bracket
    (mapM (newCString . compileFlagToArg) opts)
    (mapM free)
    (\args -> nothingIfOk =<< withArrayLen args (nvvmCompileProgram prg))

{-# INLINE nvvmCompileProgram #-}
{# fun unsafe nvvmCompileProgram
  { useProgram  `Program'
  , cIntConv    `Int'
  , id          `Ptr CString'
  }
  -> `Status' cToEnum #}


-- | Retrieve the result of compiling a 'Program' as a 'ByteString' containing
-- the generated PTX. An exception is raised if the compilation unit is invalid.
--
compilerResult :: Program -> IO ByteString
compilerResult prg = do
  n     <- resultIfOk =<< nvvmGetCompiledResultSize prg
  log   <- B.mallocByteString n
  nothingIfOk =<< nvvmGetCompiledResult prg log
  return $! B.fromForeignPtr log 0 (n-1) -- size includes the NULL terminator

{-# INLINE nvvmGetCompiledResult #-}
{# fun unsafe nvvmGetCompiledResult
  { useProgram       `Program'
  , withForeignPtr'* `ForeignPtr Word8'
  }
  -> `Status' cToEnum #}
  where
    withForeignPtr' p f = withForeignPtr (castForeignPtr p) f

{-# INLINE nvvmGetCompiledResultSize #-}
{# fun unsafe nvvmGetCompiledResultSize
  { useProgram  `Program'
  , alloca-     `Int'     peekIntConv*
  }
  -> `Status' cToEnum #}


-- | Retrieve the log of compiling a 'Program' as a 'ByteString'.
--
compilerLog :: Program -> IO ByteString
compilerLog prg = do
  n     <- resultIfOk =<< nvvmGetProgramLogSize prg
  log   <- B.mallocByteString n
  nothingIfOk =<< nvvmGetProgramLog prg log
  return $! B.fromForeignPtr log 0 (n-1) -- size includes the NULL terminator

{-# INLINE nvvmGetProgramLog #-}
{# fun unsafe nvvmGetProgramLog
  { useProgram       `Program'
  , withForeignPtr'* `ForeignPtr Word8'
  }
  -> `Status' cToEnum #}
  where
    withForeignPtr' p f = withForeignPtr (castForeignPtr p) f

{-# INLINE nvvmGetProgramLogSize #-}
{# fun unsafe nvvmGetProgramLogSize
  { useProgram  `Program'
  , alloca-     `Int'     peekIntConv*
  }
  -> `Status' cToEnum #}


-- | Verify the NVVM program. If an error is encountered, an exception is
-- thrown.
--
verifyProgram :: Program -> [VerifyFlag] -> IO ()
verifyProgram prg opts =
  bracket
    (mapM (newCString . verifyFlagToArg) opts)
    (mapM free)
    (\args -> nothingIfOk =<< withArrayLen args (nvvmVerifyProgram prg))

{# fun unsafe nvvmVerifyProgram
  { useProgram `Program'
  , cIntConv   `Int'
  , id         `Ptr CString'
  }
  -> `Status' cToEnum #}

