{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST.Global as G

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC

import Control.Monad.State
import Control.Applicative

import Data.String
import Data.Word
import Data.List
import Data.Function
import qualified Data.Map as Map

emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = label }

newtype LLVM a = LLVM { unLLVM :: State Module a }
  deriving (Functor, Applicative, Monad, MonadState Module)

runLLVM :: Module -> LLVM a -> Module
runLLVM = flip (execState . unLLVM)

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

addPrivConst :: Name -> T.Type -> C.Constant -> LLVM ()
addPrivConst name ty value = addDefn $
  GlobalDefinition $ globalAliasDefaults {
    name        = name
  , linkage     = L.Private
  , G.type'       = ty
  , aliasee     = value
  }

-- # Names

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName name names =
  case Map.lookup name names of
    Nothing -> (name, Map.insert name 1 names)
    Just ix -> (name ++ show ix, Map.insert name (ix+1) names) 

instance IsString Name where
  fromString = Name . fromString

-- # Codegen & block state

type BlocksState = Map.Map Name BlockState
type SymbolTable = [(String, Operand)]
type SingleConstDef = (Name, T.Type, C.Constant)
type ConstDef = [SingleConstDef]

data CodegenState
  = CodegenState {
    currentBlock :: Name                   -- current active block in codegen process
  , blocks       :: BlocksState            -- blocks for function/method
  , symtab       :: SymbolTable            -- function/method scope symbol table
  , blockCount   :: Int                    -- count of basic blocks
  , count        :: Word                   -- count of unnamed instructions
  , names        :: Names                  -- unique name supply
  , consts       :: ConstDef               -- constant that need to be defined as private global
  } deriving Show

data BlockState
  = BlockState {
    idx    :: Int                          -- block index
  , stack  :: [Named Instruction]          -- instruction stack
  , term   :: Maybe (Named Terminator)     -- block terminator
  } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

-- # Block functions

entry :: Codegen Name
entry = gets currentBlock

emptyBlock :: Int -> BlockState
emptyBlock idx = BlockState { idx = idx
                            , stack = []
                            , term = Nothing
                            }

addBlock :: String -> Codegen Name
addBlock name = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (uniqName, supply) = uniqueName name nms

  modify $ \s -> s { blocks = Map.insert (Name uniqName) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }

  return $ Name uniqName

setBlock :: Name -> Codegen Name
setBlock name = do
  modify $ \s -> s { currentBlock = name }
  return name

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

addConstDef :: Name -> T.Type -> C.Constant -> Codegen ()
addConstDef name ty value = do
  currentConsts <- gets consts
  modify $ \s -> s { consts = [(name, ty, value)] ++ currentConsts }

getConstants :: CodegenState -> ConstDef
getConstants (CodegenState _ _ _ _ _ _ consts) = consts

-- # LLVM instructions

entryBlockName :: String
entryBlockName = "entry"

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty []

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  let next = 1 + i
  modify $ \s -> s { count = next }
  return $ next

local :: T.Type -> Name -> Operand
local ty = LocalReference ty

global :: T.Type -> Name -> Operand
global ty = ConstantOperand . C.GlobalReference ty

funref :: Name -> Operand
funref = ConstantOperand . C.GlobalReference T.void

-- # Arithmetic

iadd :: Operand -> Operand -> Codegen Operand
iadd a b = instr $ Add False False a b []

isub :: Operand -> Operand -> Codegen Operand
isub a b = instr $ Sub False False a b []

imul :: Operand -> Operand -> Codegen Operand
imul a b = instr $ Mul False False a b []

idiv :: Operand -> Operand -> Codegen Operand
idiv a b = instr $ SDiv False a b []

imod :: Operand -> Operand -> Codegen Operand
imod a b = instr $ SRem a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map $ \x -> (x, [])

-- # Vars

assign :: String -> Operand -> Codegen ()
assign var operand = do
  currentSymtab <- gets symtab
  modify $ \s -> s { symtab = [(var, operand)] ++ currentSymtab }

tmpname :: String -> Codegen String
tmpname name = do
  nms <- gets names
  let (uniqName, supply) = uniqueName name nms
  modify $ \s -> s { names = supply }
  return uniqName

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

ptrof :: Operand -> Codegen Operand
ptrof op = instr $ GetElementPtr True op [cons $ C.Int 64 0, cons $ C.Int 64 0] []

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local T.i64 ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-- # Effects

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call False CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

-- # Control flow

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

voidRet :: Codegen (Named Terminator)
voidRet = terminator $ Do $ Ret Nothing []
