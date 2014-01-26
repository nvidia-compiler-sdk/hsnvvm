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
  Result (..), compilationLog,
  -- * Compilation
  compileModule, compileModules
) where

import Prelude hiding (log)
import Data.ByteString
import Foreign.LibNVVM.Error
import Foreign.LibNVVM.Internal

import qualified Control.Exception as E (bracket, catch, try)

-- |
-- The functions 'compileModule' and 'compileModules' return 'Result' that
-- contains the 'ErrorCode' for the compilation, the compilation log, and the
-- compiled result.
--
data Result = Result ByteString (Either ErrorCode ByteString)

-- |
-- Retrieve any compilation or verifier messages and warnings generated from
-- compiling the NVVM modules. Note that even upon successful compilation, the
-- log may be not be empty.
--
compilationLog :: Result -> ByteString
compilationLog (Result log _) = log


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
              -> String     -- ^ name of the NVVM IR module
              -> [String]   -- ^ compiler options
              -> IO Result
compileModule m n = compileModules [(m,n)]


-- |
-- The function 'compileModules' compiles and links multiple NVVM IR modules,
-- according to the specified options, and returns the compilation log and the
-- compiled result.
--
-- Each NVVM IR module can be either in the bitcode representation or in the
-- text representation. The compiled result is represented in PTX.
--
-- See 'compileModule' for the valid compiler options.
--
compileModules
    :: [(ByteString, String)]   -- ^ NVVM IR modules either in the bitcode
                                --   or text representation, together with the
                                --   module name
    -> [String]                 -- ^ compiler options
    -> IO Result
compileModules ms options =
  E.bracket create destroy $ \prog -> do
    res <- E.try $ do
              mapM_ (uncurry $ addModule prog) ms
              compile prog options
              getCompiledResult prog
    --
    log <- getProgramLog prog
    return $ case res of
      Right p'                   -> Result log (Right p')
      Left  (LibNVVMException e) -> Result log (Left e)
  `E.catch`
    \(LibNVVMException e) -> return $ Result empty (Left e)

