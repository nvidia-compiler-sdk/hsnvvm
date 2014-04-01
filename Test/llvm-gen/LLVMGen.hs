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

-- Test case that generates LLVM-IR in memory, compiles with libNVVM, and
-- executes with the CUDA FFI bindings using the driver API.
--
-- NOTE:
--   Due to the handling of the 'datalayout' field by nvvm, which appears to
--   check for an exact string match instead of parsing the value, the
--   llvm-general-pure package was edited to ensure the fields are output in the
--   required order. See the enclosed diff 'datalayout.patch'.
--

module Main (main) where

-- friends
import HelloWorld

-- llvm-general
import LLVM.General
import LLVM.General.Context
import qualified LLVM.General.AST               as AST

-- libNVVM binding
import qualified Foreign.LibNVVM                as NVVM

-- CUDA binding
import qualified Foreign.CUDA.Driver            as CUDA

-- standard library
import Data.ByteString.Char8                    as B
import Control.Monad.Error


main :: IO ()
main = withContext $ \llvm -> do
  -- (1) generate LLVM using llvm-general
  ll                    <- either error B.pack `fmap` runErrorT (withModuleFromAST llvm helloWorld moduleString)

  -- (2) compile the llvm using NVVM
  NVVM.Result log res   <- NVVM.compileModule ll "HelloWorld" []
  let fail c            = error $ "libNVVM exited with code: " ++ show c
                          ++ "\n\n  Message log:\n"
                          ++ B.unpack log
                          ++ "\n\n  Generated LLVM:\n"
                          ++ B.unpack ll
  bc                    <- either fail return res

  -- (3) initialise a CUDA context, load the module, and invoke the kernel
  CUDA.initialise []
  dev                   <- CUDA.device 0
  ctx                   <- CUDA.create dev []
  mdl                   <- CUDA.loadData bc
  hello                 <- CUDA.getFun mdl "helloWorld"
  CUDA.launchKernel hello (1,1,1) (1,1,1) 0 Nothing []
  CUDA.destroy ctx


-- Lower a Haskell LLVM AST into a C++ objects. Then, generate LLVM assembly
-- from the module.
--
llvmOfModule :: AST.Module -> IO String
llvmOfModule m =
  fmap (either error id)
       (withContext $ \ctx -> runErrorT $ withModuleFromAST ctx m moduleString)

