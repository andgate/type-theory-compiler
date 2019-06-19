{-# LANGUAGE LambdaCase
           , FlexibleContexts
           , RankNTypes
           , ConstraintKinds
           , TupleSections
           #-}
module Language.STLC.Lifted.Desugar where


import Language.STLC.Lifted.Syntax
import qualified Language.LLTT.Syntax as LL

import Control.Monad
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.Bitraversable

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name (name2String, s2n)

-- This pass produces a desugared STLC syntax tree.
-- The end result of desugaring is a Low-Level Type
-- Theory (LLTT) ast.
-- This is a form of the STLC which can be easily
-- translated into LLVM IR.

desugarModule :: [Defn] -> [LL.Defn]
desugarModule = runFreshM . mapM desugarDefn


desugarDefn :: Fresh m => Defn -> m LL.Defn
desugarDefn = \case
  FuncDefn f -> LL.FuncDefn <$> desugarFunc f
  ExternDefn ex -> LL.ExternDefn <$> desugarExtern ex
  DataTypeDefn dt -> LL.DataTypeDefn <$> desugarDataType dt

desugarExtern :: Fresh m => Extern -> m LL.Extern
desugarExtern (Extern n paramtys retty)
  = return $ LL.Extern n (desugarType <$> paramtys) (desugarType retty)

desugarDataType :: Fresh m => DataType -> m LL.DataType
desugarDataType (DataType n dt_cons)
  = return $ LL.DataType n dt_cons'
  where dt_cons' = (second (second desugarType <$>)) <$> dt_cons      

desugarFunc :: Fresh m => Func -> m LL.Func
desugarFunc (Func ty rbnd) = do
  let (fn_n, bnd) = unrebind rbnd
      fn_n' = name2String fn_n
  (ps, body) <- unbind bnd
  let ps' = desugarPat <$> ps
  body' <- desugarExp body
  return $ LL.Func fn_n' ps' body'

desugarExp  :: Fresh m => Exp -> m LL.Exp
desugarExp (EType e ty) = desugarExp' ty e
desugarExp _ = error "Expected typed expression!"


desugarExp'  :: Fresh m => Type -> Exp -> m LL.Exp
desugarExp' ty = \case
  EVar n ->
    return $ LL.EVal . LL.VVar . name2String $ n
  
  EApp f xs -> do
    f' <- desugarExp f
    xs' <- mapM desugarExp xs
    return $ LL.ECall f' xs'
  
  ELet bnd -> do
    (rbnd, body) <- unbind bnd
    let (ps, es) = unzip [(p, e) | (p, Embed e)<- untelescope rbnd]
    case body of
      EType _ body_ty -> do
        let ps' = desugarPat <$> ps
        es' <- mapM desugarExp es
        body' <- desugarExp body
        let rhs = zip ps' es'
        return $ LL.ELet rhs body'
      
      _ -> error "desugar: unable to unbind let!"


  EType e ty -> return $ error "Unexpected type annotation encountered"


desugarVal :: Exp -> Maybe (LL.Val)
desugarVal = \case
  EVar n -> Just . LL.VVar . name2String $ n
  EApp _ _ -> Nothing
  ELet _ -> Nothing
  EType e _ -> desugarVal e
  EInt i -> Just $ LL.VInt i

desugarType :: Type -> LL.Type
desugarType = \case
  ty@(TArr a b) ->
    let (paramtys, retty) = splitType ty
    in LL.TFunc (desugarType retty) (desugarType <$> paramtys) 

  TCon n  -> LL.TCon n
  TI8     -> LL.TI8
  TI32    -> LL.TI32
  TArray i ty -> LL.TArray i (desugarType ty)
  TPtr ty -> LL.TPtr (desugarType ty)
  TString -> LL.TString
  TVoid   -> LL.TVoid
  

desugarPat :: Pat -> LL.Pat
desugarPat = \case
  PVar v     -> LL.PVar (name2String v)
  PCon n ps  -> LL.PCon n (desugarPat <$> ps)
  PWild      -> LL.PWild
  PType p ty -> LL.PType (desugarPat p) (desugarType ty)