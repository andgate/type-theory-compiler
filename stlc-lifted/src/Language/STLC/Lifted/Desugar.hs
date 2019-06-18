{-# LANGUAGE LambdaCase
           , FlexibleContexts
           , RankNTypes
           , ConstraintKinds
           , TupleSections
           #-}
module Language.STLC.Lifted.Desugar where


import Language.STLC.Lifted.Syntax
import qualified Language.STLC.Core.Syntax as Core

import Control.Monad
import Data.Maybe
import Data.List

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name (name2String, s2n)

-- This pass produces a desugared STLC syntax tree.
-- The end result of desugaring is STLC Core.
-- This is a form of the STLC which can be easily
-- translated into LLVM IR.


desugarFunc :: Fresh m => Func -> m Core.Func
desugarFunc (Func ty rbnd) = do
  let (paramtys, retty) = splitType ty
  let (fn_n, bnd) = unrebind rbnd
  (ps, body) <- unbind bnd
  paramtys' <- mapM desugarType paramtys
  retty' <- desugarType retty
  when (length paramtys' /= length ns)
       $ error $ "arity mismatch in function call: " <> fn_n
  let params' = zip (name2String <$> ns)  paramtys'
  (body', caps) <- desugarExp body
  body'' <- bindCaptures caps body' retty'
  return $ Core.Func fn_n params' body'' retty'

desugarExp  :: Fresh m => Exp -> m (Core.Exp, Captures)
desugarExp (EType e ty) = desugarExp' ty e
desugarExp _ = error "Expected typed expression!"


desugarExp'  :: Fresh m => Type -> Exp -> m Core.Exp
desugarExp' ty = \case
  EVar n ->
    return (Core.EVal . Core.VVar . name2String $ n, [])
  
  EApp f xs -> do
    f' <- desugarExp f
    xs' <- mapM desugarExp xs
    return $ Core.ECall f' xs'
  
  ELet bnd -> do
    (rbnd, body) <- unbind bnd
    let (ps, es) = unzip [(p, e) | (p, Embed e)<- untelescope rbnd]
    case body of
      EType _ body_ty -> do
        es' <- mapM desugarExp es
        body' <- desugarExp body
        let rhs = zip (desugarPat <$> ps) es'
        return $ Core.ELet rhs body'
      
      _ -> error "desugar: unable to unbind let!"


  EType e ty -> return $ error "Unexpected type annotation encountered"


desugarVal :: Exp -> Maybe (Core.Val)
desugarVal = \case
  EVar n -> Just . Core.VVar . name2String $ n
  EApp _ _ -> Nothing
  ELet _ -> Nothing
  EType e _ -> desugarVal e
  EInt i -> Just $ Core.VInt i

desugarType :: Fresh m => Type -> m Core.Type
desugarType = \case
  ty@(TArr a b) ->
    let (paramtys, retty) = splitType ty
    in Core.TFunc <$> desugarType retty <*> mapM desugarType paramtys 

  TCon n  -> pure $ Core.TCon n
  TI8     -> pure Core.TI8
  TI32    -> pure Core.TI32
  TArray i ty -> Core.TArray i <$> desugarType ty
  TPtr ty -> Core.TPtr <$> desugarType ty
  TString -> pure Core.TString
  TVoid   -> pure Core.TVoid
  

desugarPattern :: Pat -> [Core.Pat]
desugarPattern 