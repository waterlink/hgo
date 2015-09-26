module Types where

import qualified LLVM.General.AST.Type as T

i8 = T.i8
i16 = T.i16
i32 = T.i32
i64 = T.i64
i128 = T.i128

f16 = T.half
f32 = T.float
f64 = T.double
f128 = T.fp128

ptr = T.ptr

void = T.void

char = i8
cstring = ptr char

array = T.ArrayType
