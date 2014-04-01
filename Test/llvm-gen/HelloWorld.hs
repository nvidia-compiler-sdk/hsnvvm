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

-- LLVM AST for the helloWorld program
--
module HelloWorld (helloWorld) where

-- llvm-general-pure
import LLVM.General.AST
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Attribute
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant                as Const
import LLVM.General.AST.DataLayout
import LLVM.General.AST.Linkage
import LLVM.General.AST.Visibility

-- standard library
import Data.Bits
import Data.Char
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


moduleHeader :: String -> [Definition] -> Module
moduleHeader name defs
  = let
        word    = fromIntegral $ bitSize (undefined :: Int)
        layout  = Just $
          DataLayout
            { endianness        = Just LittleEndian
            , stackAlignment    = Nothing
            , pointerLayouts    = Map.fromList
                [ (AddrSpace 0, (word, AlignmentInfo word (Just word)))  -- changes depending on host
                ]
            , typeLayouts       = Map.fromList $                        -- always same
                [ ((IntegerAlign, 1), AlignmentInfo 8 (Just 8)) ] ++
                [ ((IntegerAlign, i), AlignmentInfo i (Just i)) | i <- [8,16,32,64]] ++
                [ ((VectorAlign,  v), AlignmentInfo v (Just v)) | v <- [16,32,64,128]] ++
                [ ((FloatAlign,   f), AlignmentInfo f (Just f)) | f <- [32,64]]
            , nativeSizes       = Just $ Set.fromList [ 16,32,64 ]      -- always same
            }
        target  = Just "nvptx-nvidia-cl.1.0"
    in
    Module name layout target defs


helloWorld :: Module
helloWorld
  = moduleHeader "HelloWorld"
  $ [str, used, hello, cast, vprintf] ++ annotations
  where
    cstring :: String -> Constant
    cstring s =
      Array (IntegerType 8)
            [Int 8 (fromIntegral (ord c)) | c <- (s ++ "\0")]     -- NULL terminate

    str = GlobalDefinition $ GlobalVariable (Name "str")
      Private                                   -- linkage
      Default                                   -- visibility
      False                                     -- is thread local
      (AddrSpace 4)                             -- address space (4 = constant)
      False                                     -- has unnamed address
      True                                      -- is constant
      (ArrayType 16 (IntegerType 8))            -- type
      (Just (cstring "Hello World!!!\n"))
      Nothing                                   -- section
      0                                         -- alignment

    vprintf = GlobalDefinition $ Function
      External                                  -- linkage
      Default                                   -- visibility
      C                                         -- calling convention (all accepted and ignored)
      []                                        -- return attributes
      (IntegerType 32)                          -- return type
      (Name "vprintf")                          -- name
      ([Parameter (PointerType (IntegerType 8) (AddrSpace 0)) (UnName 0) [NoCapture]
       ,Parameter (PointerType (IntegerType 8) (AddrSpace 0)) (UnName 1) [] ], False) -- parameters, snd indicates varargs
      [NoUnwind]                                -- function attributes
      Nothing                                   -- section
      0                                         -- alignment
      Nothing                                   -- garbage collector name
      []                                        -- basic blocks

    cast = GlobalDefinition $ Function
      External
      Default
      C
      []
      (PointerType (IntegerType 8) (AddrSpace 0))
      (Name "llvm.nvvm.ptr.constant.to.gen.p0i8.p4i8")
      ([Parameter (PointerType (IntegerType 8) (AddrSpace 4)) (UnName 0) [NoCapture]], False)
      [NoUnwind, ReadNone]
      Nothing
      0
      Nothing
      []

    -- The hello world kernel function
    hello = GlobalDefinition $ Function
      External Default C [] VoidType (Name "helloWorld") ([], False) [] Nothing 0 Nothing
      [BasicBlock (Name "entry")
        -- Copy the constant string (AddrSpace 4) into code space (AddrSpace 0) ?
        [UnName 0 := Call True        -- is tail call
                          C           -- calling convention (ignored)
                          []          -- return attributes
                          (Right (ConstantOperand (GlobalReference (Name "llvm.nvvm.ptr.constant.to.gen.p0i8.p4i8"))))
                                      -- callable operand
                          [(ConstantOperand (Const.GetElementPtr True (GlobalReference (Name "str")) [Int 64 0, Int 64 0]),[])]
                                      -- arguments (operand, parameters)
                          []          -- function attributes
                          []          -- instruction metadata

        -- Call vprintf
        ,UnName 1 := Call True C [] (Right (ConstantOperand (GlobalReference (Name "vprintf"))))
                                    [ (LocalReference (UnName 0), [])
                                    , (ConstantOperand (Null (PointerType (IntegerType 8) (AddrSpace 0))), [])
                                    ]
                                    []
                                    []
        ]
        (Do (Ret Nothing []))]

    -- Collect the metadata into the nnvm.annotations
    annotations
      = NamedMetadataDefinition "nvvm.annotations" [MetadataNodeID 0]
      : NamedMetadataDefinition "nvvm.version"     [MetadataNodeID 1]
      : [version, kernels]
      where
        version = MetadataNodeDefinition (MetadataNodeID 1)
          [ Just $ ConstantOperand (Int 32 1)
          , Just $ ConstantOperand (Int 32 0) ]

        kernels = MetadataNodeDefinition (MetadataNodeID 0)
          [ Just $ ConstantOperand (GlobalReference (Name "helloWorld"))
          , Just $ MetadataStringOperand "kernel"
          , Just $ ConstantOperand (Int 32 1)]

    -- Intrinsic global variable. Contains a list of pointers to named global
    -- variables, functions and aliases which may optionally have a pointer cast
    -- form of bitcast or getelementptr.
    --
    -- If a symbol appears in the list, then the compiler is required to treat
    -- the symbol as if there is a reference to the symbol that it cannot see.
    -- E.g. if a variable has internal linkage and no references other than that
    -- from @llvm.used list, it can not be deleted.
    --
    used = GlobalDefinition $ GlobalVariable
      (Name "llvm.used")
      Appending
      Default
      False
      (AddrSpace 0)                     -- address space (0 = code)
      False
      False
      (ArrayType 1 (PointerType (IntegerType 8) (AddrSpace 0)))
      (Just $ Array (PointerType (IntegerType 8) (AddrSpace 0))
                [ Const.BitCast (GlobalReference (Name "helloWorld"))
                                (PointerType (IntegerType 8) (AddrSpace 0)) ])
      (Just "llvm.metadata")
      0

