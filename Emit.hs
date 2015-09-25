module Emit where

import Control.Monad.Except
import Control.Applicative

import qualified LLVM.General.Module as Module
import qualified LLVM.General.Context as Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as T

import qualified Data.Map as Map

import Codegen
import qualified Syntax as S

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegenTop :: S.TopLevelDecl -> LLVM ()
codegenTop (S.FunctionDecl name (S.Signature args result) (Just body)) = do
  define resultType name funcArgs blocks
  where
    resultType = resultTypeOf result
    funcArgs = funcArgsOf args
    blocks = blocksFor body
    funcArgsOf = concat $ map funcArgOf

    -- FIXME: supports currently only void and int
    resultTypeOf Nothing = T.void
    resultTypeOf (Just (S.FunctionSingleResult _)) = T.i64

    -- FIXME:
    -- - ignores type
    -- - does not implement ellipsis
    funcArgOf (S.ParameterDecl names _ Nothing) = map $ \name -> (T.i64, name)

    -- FIXME: ignores provided list of statements entirely
    blocksFor _ = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry

codegen :: AST.Module -> [S.TopLevelDecl] -> IO AST.Module
codegen mod decls = Context.withContext $ \context ->
  liftError $ Module.withModuleFromAST context newast $ \m -> do
    llstr <- Module.moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop decls
    newast  = runLLVM mod modn
