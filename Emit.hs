module Emit where

import Control.Monad.Except
import Control.Applicative

import qualified LLVM.General.Module as Module
import qualified LLVM.General.Context as Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as LT

import Data.Char
import qualified Data.Map as Map

import Codegen
import JIT
import qualified Syntax as S
import qualified Types as T

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegenTop :: S.TopLevelDecl -> LLVM ()
codegenTop (S.FunctionDecl name (S.Signature args result) (Just body)) = do
  mapM defineConst constants
  define resultType name funcArgs blocks
  where
    resultType = resultTypeOf result
    funcArgs = funcArgsOf args
    (blocks, constants) = blocksFor body funcArgs

codegenTop (S.ExternFunctionDecl name (S.Signature args result)) = do
  external resultType name funcArgs
  where
    resultType = resultTypeOf result
    funcArgs = funcArgsOf args

codegen :: AST.Module -> [S.TopLevelDecl] -> IO AST.Module
codegen mod decls = do
  res <- runJIT oldast
  case res of
    Right newast -> return newast
    Left err     -> putStrLn err >> return oldast
  where
    modn    = mapM codegenTop decls
    oldast  = runLLVM mod modn

-- # codegen expressions and statements

cgen :: S.Statement -> Codegen AST.Operand
cgen (S.SimpleStmt (S.ExpressionStmt expr)) = cgenexpr expr

cgen statement = error $ "Unable to understand statement: " ++ show statement

cgenexpr :: S.Expression -> Codegen AST.Operand
cgenexpr (S.Binary (S.AddBinOp S.Plus) a b) = cgenbinop iadd a b
cgenexpr (S.Binary (S.AddBinOp S.Minus) a b) = cgenbinop isub a b

cgenexpr (S.Binary (S.MulBinOp S.Mult) a b) = cgenbinop imul a b
cgenexpr (S.Binary (S.MulBinOp S.Div) a b) = cgenbinop idiv a b
cgenexpr (S.Binary (S.MulBinOp S.Mod) a b) = cgenbinop imod a b

cgenexpr (S.Unary (S.PrimaryExpr (S.Arguments f args _ _))) = cgencall f args

cgenexpr (S.Unary (S.PrimaryExpr (S.Operand (S.Literal (S.IntLiteral value))))) = return $ cons $ C.Int 64 value
cgenexpr (S.Unary (S.PrimaryExpr (S.Operand (S.Literal (S.StringLiteral value))))) = cgencstring value

cgenexpr (S.Unary (S.PrimaryExpr (S.Operand (S.OperandName (S.OperandIdentifier name))))) = getvar name

cgenexpr expr = error $ "Unable to understand expression: " ++ show expr

cgenfn :: S.PrimaryExpr -> Codegen AST.Operand
cgenfn (S.Operand (S.OperandName (S.OperandIdentifier name))) = return $ funref $ AST.Name name

cgenbinop :: (AST.Operand -> AST.Operand -> Codegen AST.Operand) -> S.Expression -> S.Expression -> Codegen AST.Operand
cgenbinop fn a b = do
  ca <- cgenexpr a
  cb <- cgenexpr b
  fn ca cb

cgencall :: S.PrimaryExpr -> [S.Expression] -> Codegen AST.Operand
cgencall f args = do
  cf <- cgenfn f
  cargs <- mapM cgenexpr args
  call cf cargs

i8array :: String -> [C.Constant]
i8array [] = [C.Int 8 0]
i8array (x:xs) = (C.Int 8 $ toInteger $ ord x):(i8array xs)

cgencstring :: String -> Codegen AST.Operand
cgencstring value = do
  var <- alloca $ T.array (fromIntegral $ length strvalue) T.char
  store var strconst

  ptr <- ptrof var
  return ptr
  where
    strconst = cons $ C.Array T.char $ i8array value
    strvalue = i8array value

-- # Function generation helpers

funcArgsOf args = concat $ map funcArgOf args

-- FIXME: supports currently only void and int
resultTypeOf Nothing = T.void
resultTypeOf (Just (S.FunctionSingleResult _)) = T.i64

-- FIXME:
-- - does not implement ellipsis
funcArgOf (S.ParameterDecl names ty Nothing) = map (\name -> (getType ty, AST.Name name)) names

-- FIXME: ignores provided list of statements entirely
blocksFor :: [S.Statement] -> [(LT.Type, AST.Name)] -> ([AST.BasicBlock], ConstDef)
blocksFor body funcArgs =
  (blocks, constants)
  where
    code = do
      entry <- addBlock entryBlockName
      setBlock entry
      forM funcArgs $ \(ty, (AST.Name name)) -> do
        assign name $ local ty $ AST.Name name
      forM (filter nonEmpty body) cgen
      voidRet

    executed = execCodegen code
    blocks = createBlocks executed
    constants = getConstants executed
    nonEmpty (S.SimpleStmt S.Empty) = False
    nonEmpty _ = True

getType :: S.Type -> LT.Type
getType (S.TypeName (S.Identifier "cstring")) = T.cstring
getType (S.TypeName (S.Identifier "int")) = T.i64
getType (S.TypeLiteral (S.InterfaceType [])) = T.ptr T.void

getType x = error $ "Unable to recognize type: " ++ show x

defineConst :: SingleConstDef -> LLVM ()
defineConst (name, ty, value) = addPrivConst name ty value
