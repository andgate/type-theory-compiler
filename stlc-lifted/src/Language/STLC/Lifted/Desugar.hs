{-# LANGUAGE LambdaCase
           , FlexibleContexts
           , RankNTypes
           , ConstraintKinds
           , TupleSections
           , ViewPatterns
           #-}
module Language.STLC.Lifted.Desugar where


import Language.STLC.Lifted.Syntax
import qualified Language.LLTT.Syntax as LL

import Control.Monad
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Bitraversable

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name (name2String, s2n)

-- This pass produces a desugared STLC syntax tree.
-- The end result of desugaring is a Low-Level Type
-- Theory (LLTT) ast.
-- This is a form of the STLC which can be easily
-- translated into LLVM IR.

desugarModule :: Module -> LL.Module
desugarModule (Module n defns)
  = LL.Module n $ runFreshM $ mapM desugarDefn defns


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
desugarFunc (Func ty fn_n bnd) = do
  (ps, body) <- unbind bnd
  let ps' = desugarPat <$> ps
  body' <- desugarExp body
  return $ LL.Func fn_n ps' body'

desugarExp  :: Fresh m => Exp -> m LL.Exp
desugarExp (EType e ty) = LL.EType <$> desugarExp' ty e <*> pure (desugarType ty)
desugarExp e = error $ "desugarExp - Expected typed expression:\n\n" ++ show e ++ "\n\n"


desugarExp'  :: Fresh m => Type -> Exp -> m LL.Exp
desugarExp' ty = \case
  EVar n ->
    return $ LL.EVal . LL.VVar . name2String $ n

  EType e ty' -> LL.EType <$> desugarExp' ty' e <*> pure (desugarType ty')
  
  EApp f xs -> do
    f' <- desugarExp f
    xs' <- mapM desugarExp xs
    return $ LL.ECall f' xs'
  
  ELet bnd -> do
    (unrec -> letbnds, body) <- unbind bnd
    let (ps, es) = unzip (second unembed <$> letbnds)
    let ps' = desugarPat <$> ps
    es' <- mapM desugarExp es
    let rhs = zip ps' es'
    body' <- desugarExp body
    return $ LL.ELet rhs body'

  ECase e@(EType _ TI32) cls -> do
    error "Integer case not supported"

  ECase e cls -> do
    e' <- desugarExp e
    cls' <- NE.fromList <$> mapM desugarClause cls
    return $ LL.EMatch e' cls'

  EInt i -> return $ LL.EVal $ LL.VInt i
  EString str -> return $ LL.EVal $ LL.VString str

  ECon n xs -> do
    xs' <- mapM desugarExp xs
    return $ LL.EOp $ LL.MemOp $ LL.ConstrOp $ LL.Constr n xs'

  ENewCon n xs -> do
    xs' <- mapM desugarExp xs
    return $ LL.EOp $ LL.MemOp $ LL.NewOp $ LL.Constr n xs'

  EFree e -> do
    e' <- desugarExp e
    return $ LL.EOp $ LL.MemOp $ LL.FreeOp e'

  EDeref e -> do
    e' <- desugarExp e
    return $ LL.EOp $ LL.PtrOp $ LL.DerefOp e'

  ERef e -> do
    e' <- desugarExp e
    return $ LL.EOp $ LL.PtrOp $ LL.RefOp e'

  EMember e m -> do
    e' <- desugarExp e
    return $ LL.EOp $ LL.MemOp $ LL.MemAccess e' m

  EOp op -> LL.EOp <$> desugarOp op

  e -> error $ "unhandled case: " ++ show e

desugarOp :: Fresh m => Op -> m LL.Op
desugarOp = \case
  OpAddI a b -> LL.IArithOp <$> (LL.AddOpI <$> desugarExp a <*> desugarExp b)
  OpMulI a b -> LL.IArithOp <$> (LL.MulOpI <$> desugarExp a <*> desugarExp b)

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


desugarClause :: Fresh m => Clause -> m (String, [Maybe String], LL.Exp)
desugarClause (Clause bnd) = do
  (p, e) <- unbind bnd
  let ns = (Just . fst) <$> patTypedVars p
  case p of
    PType (PCon n args) _ -> do
      e' <- desugarExp e
      return (n, ns, e')

    p -> error $ "typed pattern expected, instead found:\n\n" ++ show p ++ "\n\n"

