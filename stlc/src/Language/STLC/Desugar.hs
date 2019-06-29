{-# LANGUAGE LambdaCase
           , FlexibleContexts
           , RankNTypes
           , ConstraintKinds
           , TupleSections
           , ViewPatterns
           #-}
module Language.STLC.Desugar where


import Language.STLC.Syntax
import qualified Language.LLTT.Syntax as LL

import Language.STLC.Reduce

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
    return $ LL.EVar . name2String $ n

  ELit l -> LL.ELit <$> desugarLit l

  EType e ty' -> LL.EType <$> desugarExp' ty' e <*> pure (desugarType ty')
  
  EApp f xs -> do
    f' <- desugarExp f
    xs' <- mapM desugarExp xs
    return $ LL.ECall f' (NE.fromList xs')
  
  ELet bnd -> do
    (unrec -> letbnds, body) <- unbind bnd
    let (ps, es) = unzip (second unembed <$> letbnds)
    let ps' = desugarPat <$> ps
    es' <- mapM desugarExp es
    let rhs = zip ps' es'
    body' <- desugarExp body
    return $ LL.ELet (NE.fromList rhs) body'

  EIf p t f -> LL.EIf <$> desugarExp p <*> desugarExp t <*> desugarElse f

  ECase e@(EType _ TI32) cls -> do
    error "Integer case not supported"

  ECase e cls -> do
    e' <- desugarExp e
    cls' <- NE.fromList <$> mapM desugarClause cls
    return $ LL.EMatch e' cls'

  ERef e ->
    LL.ERef <$> desugarExp e
  
  EDeref e -> 
    LL.EDeref <$> desugarExp e


  ECon n xs ->
    LL.ECon n <$> mapM desugarExp xs

  ENewCon n xs ->
    LL.ENewCon n <$> mapM desugarExp xs

  EFree e -> 
    LL.EFree <$> desugarExp e

  
  EGet e m ->
    LL.EGet <$> desugarExp e <*> pure m

  EGetI e i -> LL.EGetI <$> desugarExp e <*> desugarExp i

  ESet lhs rhs -> LL.ESet <$> desugarExp lhs <*> desugarExp rhs

  ENewArray xs -> LL.ENewArray <$> mapM desugarExp xs

  ENewArrayI i -> LL.ENewArrayI <$> desugarExp i

  ENewStringI i -> LL.ENewStringI <$> desugarExp i

  EOp op -> LL.EOp <$> desugarOp op

  e -> error $ "unhandled case: " ++ show e


desugarLit :: Fresh m => Lit -> m LL.Lit
desugarLit = \case
  LInt i     -> pure $ LL.LInt i
  LDouble d  -> pure $ LL.LDouble d
  LChar c    -> pure $ LL.LChar c
  LBool b    -> pure $ LL.LBool b
  LString s  -> pure $ LL.LString s
  LStringI i ->
    case reduceBy mempty 50 i of
      EType (ELit (LInt i)) _ -> return $ LL.LStringI i
      ELit (LInt i) -> return $ LL.LStringI i
      _ -> error $ "desugar - expected constant integer! Found: " ++ show i

  LArray xs  -> LL.LArray   <$> mapM desugarExp xs
  LArrayI i  ->
    case reduceBy mempty 50 i of
      EType (ELit (LInt i)) _ -> return $ LL.LArrayI i
      ELit (LInt i) -> return $ LL.LArrayI i
      _ -> error $ "desugar - expected constant integer! Found: " ++ show i


desugarElse :: Fresh m => Else -> m LL.Else
desugarElse = \case
  Else e -> LL.Else <$> desugarExp e
  Elif p t f -> LL.Elif <$> desugarExp p <*> desugarExp t <*> desugarElse f


desugarOp :: Fresh m => Op -> m LL.Op
desugarOp = \case
  OpAddI a b -> LL.OpAddI <$> desugarExp a <*> desugarExp b
  OpSubI a b -> LL.OpSubI <$> desugarExp a <*> desugarExp b
  OpMulI a b -> LL.OpMulI <$> desugarExp a <*> desugarExp b

  OpAddF a b -> LL.OpAddF <$> desugarExp a <*> desugarExp b
  OpSubF a b -> LL.OpSubF <$> desugarExp a <*> desugarExp b
  OpMulF a b -> LL.OpMulF <$> desugarExp a <*> desugarExp b

  OpEqI a b -> LL.OpEqI <$> desugarExp a <*> desugarExp b
  OpNeqI a b -> LL.OpNeqI <$> desugarExp a <*> desugarExp b

desugarType :: Type -> LL.Type
desugarType = \case
  ty@(TArr a b) ->
    let (paramtys, retty) = splitType ty
    in LL.TFunc (desugarType retty) (NE.fromList $ desugarType <$> paramtys) 

  TCon n  -> LL.TCon n
  TI8     -> LL.TI8
  TI32    -> LL.TI32
  TI64    -> LL.TI64
  TF32    -> LL.TF32
  TF64    -> LL.TF64
  TBool   -> LL.TBool
  TChar   -> LL.TChar
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


desugarClause :: Fresh m => Clause -> m LL.Clause
desugarClause (Clause bnd) = do
  (p, e) <- unbind bnd
  let ns = (Just . fst) <$> patTypedVars p
  case p of
    PType (PCon n args) _ -> do
      e' <- desugarExp e
      return $ LL.Clause n ns e'

    p -> error $ "typed pattern expected, instead found:\n\n" ++ show p ++ "\n\n"

