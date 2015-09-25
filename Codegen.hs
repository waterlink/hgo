{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC

import Control.Monad.State
import Control.Applicative

import Data.String
import Data.Word
import qualified Data.Map as Map

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
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

data CodegenState
  = CodegenState {
    currentBlock :: Name                   -- current active block in codegen process
  , blocks       :: BlocksState            -- blocks for function/method
  , symtab       :: SymbolTable            -- function/method scope symbol table
  , blockCount   :: Int                    -- count of basic blocks
  , count        :: Word                   -- count of unnamed instructions
  , names        :: Names                  -- unique name supply
  } deriving Show

data BlockState
  = BlockState {
    idx    :: Int                          -- block index
  , stack  :: [Named Instruction]          -- instruction stack
  , term   :: Maybe (Named Terminator)     -- block terminator
  } deriving Show
  
newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

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
