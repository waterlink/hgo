module Emit where

import qualified LLVM.General.Module
import qualified LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

import qualified Data.Map as Map

import Codegen
import qualified Syntax as S

codegen :: AST.Module -> [S.TopLevelDecl] -> IO AST.Module
codegen mod decls = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop decls
    newast  = runLLVM mod modn
