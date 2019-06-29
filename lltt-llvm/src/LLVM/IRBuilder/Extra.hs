module LLVM.IRBuilder.Extra where

import Control.Monad

import LLVM.AST hiding (callingConvention)
import LLVM.AST.CallingConvention
import LLVM.AST.Global
import LLVM.AST.Type
import qualified LLVM.AST.Constant as C
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

functionCC
  :: MonadModuleBuilder m
  => Name  -- ^ Function name
  -> CallingConvention
  -> [(Type, ParameterName)]  -- ^ Parameter types and name suggestions
  -> Type  -- ^ Return type
  -> ([Operand] -> IRBuilderT m ())  -- ^ Function body builder
  -> m Operand
functionCC label cc argtys retty body = do
  let tys = fst <$> argtys
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, paramName) -> case paramName of
      NoParameterName -> fresh
      ParameterName p -> fresh `named` p
    body $ zipWith LocalReference tys paramNames
    return paramNames
  let
    def = GlobalDefinition functionDefaults
      { name        = label
      , callingConvention = cc
      , parameters  = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
      , returnType  = retty
      , basicBlocks = blocks
      }
    funty = ptr $ FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label