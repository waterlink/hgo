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

    funcArgsOf args = concat $ map funcArgOf args

    -- FIXME: supports currently only void and int
    resultTypeOf Nothing = T.void
    resultTypeOf (Just (S.FunctionSingleResult _)) = T.i64

    -- FIXME:
    -- - ignores type
    -- - does not implement ellipsis
    funcArgOf (S.ParameterDecl names _ Nothing) = map (\name -> (T.i64, (AST.Name name))) names

    -- FIXME: ignores provided list of statements entirely
    blocksFor body = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM funcArgs $ \(ty, (AST.Name name)) -> do
        var <- alloca ty
        store var $ local (AST.Name name)
        assign name var
      forM body $ \statement -> do
        result <- cgen statement
        assign "_" result
      voidRet

codegen :: AST.Module -> [S.TopLevelDecl] -> IO AST.Module
codegen mod decls = Context.withContext $ \context ->
  liftError $ Module.withModuleFromAST context newast $ \m -> do
    llstr <- Module.moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop decls
    newast  = runLLVM mod modn

cgen :: S.Statement -> Codegen AST.Operand
cgen (S.SimpleStmt (S.ExpressionStmt expr)) = cgenexpr expr

cgen statement = error $ "Unable to understand statement: " ++ show statement

cgenexpr :: S.Expression -> Codegen AST.Operand
cgenexpr (S.Binary (S.AddBinOp S.Plus) a b) = binop iadd a b
cgenexpr (S.Binary (S.AddBinOp S.Minus) a b) = binop isub a b

cgenexpr (S.Unary (S.PrimaryExpr (S.Operand (S.Literal (S.IntLiteral value))))) = return $ cons $ C.Int 64 value
cgenexpr (S.Unary (S.PrimaryExpr (S.Operand (S.OperandName (S.OperandIdentifier name))))) = getvar name

cgenexpr expr = error $ "Unable to understand expression: " ++ show expr

binop :: (AST.Operand -> AST.Operand -> Codegen AST.Operand) -> S.Expression -> S.Expression -> Codegen AST.Operand
binop fn a b = do
  ca <- cgenexpr a
  cb <- cgenexpr b
  fn ca cb
