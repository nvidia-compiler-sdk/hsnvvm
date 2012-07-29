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
  -- * Compilation result
  Result (..),
  -- * Compilation
  compileModule, compileModules
) where

import Data.ByteString
import Foreign.LibNVVM.Error
import Foreign.LibNVVM.Internal

import qualified Control.Exception as E (bracket, bracket_, catch, try)

-- |
-- The functions 'compileModule' and 'compileModules' return 'Result' that
-- contains the 'ErrorCode' for the compilation, the compilation log, and the
-- compiled result.
--
-- It is to be noted that, even upon the successful compilation,
-- 'compilationLog' may not be empty. It may contain some advisory messages
-- and warnings.
--
data Result = Result {errorCode      :: ErrorCode,
                      compilationLog :: ByteString,
                      ptx            :: ByteString}

-- |
-- The function 'compileModule' compiles an NVVM IR module, according to the
-- specified options, and returns the compilation log and the compiled result.
--
-- The NVVM IR module can be either in the bitcode representation or in the
-- text representation. The compiled result is represented in PTX.
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
compileModule :: ByteString -- ^ an NVVM IR module either in the bitcode
                            --   representation or in the text representation
              -> [String]   -- ^ compiler options
              -> IO Result
compileModule m = compileModules [m]


-- |
-- The function 'compileModules' compiles and links multiple NVVM IR modules,
-- according to the specified options, and returns the compilation log and the
-- compiled result.
-- compilation log.
--
-- Each NVVM IR module can be either in the bitcode representation or in the
-- text representation. The compiled result is represented in PTX.
--
-- See 'compileModule' for the valid compiler options.
--
compileModules :: [ByteString] -- ^ NVVM IR modules either in the bitcode
                               --   representation or in the text
                               --   representation
               -> [String]     -- ^ compiler options
               -> IO Result
compileModules ms options =
  E.catch (E.bracket_ initialize finalize $
           E.bracket  create     destroy  $ \cu ->
           E.try (mapM_ (addModule cu) ms >>
                  compile cu options >>
                  getCompiledResult cu) >>= \p ->
           getCompilationLog cu >>= \compLog -> return $ case p of
           Right p'                   -> Result Success compLog p'
           Left  (LibNVVMException e) -> Result e compLog empty)
          (\(LibNVVMException e) -> return $ Result e empty empty)
