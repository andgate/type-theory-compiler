{-# LANGUAGE LambdaCase
           , FlexibleContexts
           , RankNTypes
           , ConstraintKinds
           , TupleSections
           , ViewPatterns
           , OverloadedStrings
           #-}
module Language.STLC.Desugar where

import Language.Syntax.Location
import Language.STLC.Syntax
import Language.STLC.Pretty
import qualified Language.LLTT.Syntax as LL

import Language.STLC.Reduce

import Control.Monad
import Control.Monad.Reader
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Bitraversable

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name (name2String, s2n)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import System.IO.Unsafe
import System.Exit (exitFailure)

-- This pass produces a desugared STLC syntax tree.
-- The end result of desugaring is a Low-Level Type
-- Theory (LLTT) ast.
-- This is a form of the STLC which can be easily
-- translated into LLVM IR.

type MonadDesugar m = (Fresh m, MonadReader Loc m)

withLoc :: MonadDesugar m => Loc -> m a -> m a
withLoc l = local (const l) 

askLoc :: MonadDesugar m => m Loc
askLoc = ask

desugarModule :: Module -> LL.Module
desugarModule (Module l n defns)
  = LL.Module l n $ runFreshM $ runReaderT (mapM desugarDefn defns) l


desugarDefn :: MonadDesugar m => Defn -> m LL.Defn
desugarDefn = \case
  FuncDefn f -> LL.FuncDefn <$> desugarFunc f
  ExternDefn ex -> return $ LL.ExternDefn (desugarExtern ex)
  DataTypeDefn dt -> return $ LL.DataTypeDefn (desugarDataType dt)

desugarExtern :: Extern -> LL.Extern
desugarExtern (Extern l n paramtys retty)
  = LL.Extern l n (desugarType <$> paramtys) (desugarType retty)

desugarDataType :: DataType -> LL.DataType
desugarDataType (DataType l n cs)
  = LL.DataType l n (desugarConstrDefn <$> cs)


desugarConstrDefn :: ConstrDefn -> LL.ConstrDefn
desugarConstrDefn = \case
  ConstrDefn l n tys -> LL.ConstrDefn l n (desugarType <$> tys)
  RecordDefn l n ens -> LL.RecordDefn l n (desugarEntry <$> ens)


desugarEntry :: Entry -> LL.Entry
desugarEntry (Entry l n ty) = LL.Entry l n (desugarType ty)

desugarFunc :: MonadDesugar m => Func -> m LL.Func
desugarFunc (Func l ty fn_n bnd) = withLoc l $ do
  (ps, body) <- unbind bnd
  let ps' = desugarPat <$> ps
  body' <- desugarExp body
  return $ LL.Func l fn_n ps' body'

desugarExp  :: MonadDesugar m => Exp -> m LL.Exp
desugarExp (EType e ty) = LL.EType <$> desugarExp' ty e <*> pure (desugarType ty)
desugarExp (ELoc e l) = withLoc l $ LL.ELoc <$> desugarExp e <*> pure l
desugarExp e = do
  l <- askLoc
  return $ unsafePerformIO $ do
    putDoc $ vsep [ line <> pretty l <+> "fatal compiler error:"
                  , indent 4 $ vsep [ "desugarExp - Expected typed expression."
                                    , "in"
                                    , indent 2 $ pretty e
                                    ]
                  ]
    exitFailure


desugarExp'  :: MonadDesugar m => Type -> Exp -> m LL.Exp
desugarExp' ty = \case
  EVar n ->
    return $ LL.EVar . name2String $ n

  ELit l -> LL.ELit <$> desugarLit l

  EApp f xs ->
    LL.ECall <$> desugarExp f <*>  mapM desugarExp xs

  EType e ty' -> LL.EType <$> desugarExp' ty' e <*> pure (desugarType ty')
  ECast e ty' -> LL.ECast <$> desugarExp e <*> pure (desugarType ty')
  
  ELoc e l -> withLoc l $ LL.ELoc <$> desugarExp' ty e <*> pure l
  EParens e -> LL.EParens <$> desugarExp' ty e

  ELam bnd -> error "Unlifted lambda expression encountered!"

  ELet bnd -> do
    (unrec -> letbnds, body) <- unbind bnd
    let (ps, es) = NE.unzip (second unembed <$> letbnds)
    let ps' = desugarPat <$> ps
    es' <- mapM desugarExp es
    let rhs = NE.zip ps' es'
    body' <- desugarExp body
    return $ LL.ELet rhs body'

  EIf p t f -> LL.EIf <$> desugarExp p <*> desugarExp t <*> desugarElse f

  ECase e cls ->
    case exTyAnn (exType e) of
      TCon _ -> LL.EMatch <$> desugarExp e <*> mapM desugarClause cls
      t | t `elem` intTypes -> LL.EMatchI <$> desugarExp e <*> mapM desugarClause cls
      _ -> error "Case analysis on unsupported type!"
      -- At some point, it would be nice if case could work with any type...

  ERef e ->
    LL.ERef <$> desugarExp e
  
  EDeref e -> 
    LL.EDeref <$> desugarExp e

  ETuple x xs ->
    LL.ETuple <$> desugarExp x <*> mapM desugarExp xs

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


desugarLit :: MonadDesugar m => Lit -> m LL.Lit
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


desugarElse :: MonadDesugar m => Else -> m LL.Else
desugarElse = \case
  Else (Just l) e -> withLoc l $ desugarElse $ Else Nothing e
  Else Nothing e -> LL.Else <$> fmap Just askLoc <*> desugarExp e
  
  Elif (Just l) p t f
    -> withLoc l $ desugarElse $ Elif Nothing p t f
  Elif Nothing p t f
    -> LL.Elif <$> fmap Just askLoc <*> desugarExp p <*> desugarExp t <*> desugarElse f


desugarOp :: MonadDesugar m => Op -> m LL.Op
desugarOp = \case
  OpAddI a b -> LL.OpAddI <$> desugarExp a <*> desugarExp b
  OpSubI a b -> LL.OpSubI <$> desugarExp a <*> desugarExp b
  OpMulI a b -> LL.OpMulI <$> desugarExp a <*> desugarExp b
  OpDivI a b -> LL.OpDivI <$> desugarExp a <*> desugarExp b
  OpRemI a b -> LL.OpRemI <$> desugarExp a <*> desugarExp b
  OpNeg a -> LL.OpNeg <$> desugarExp a

  OpAddF a b -> LL.OpAddF <$> desugarExp a <*> desugarExp b
  OpSubF a b -> LL.OpSubF <$> desugarExp a <*> desugarExp b
  OpMulF a b -> LL.OpMulF <$> desugarExp a <*> desugarExp b
  OpDivF a b -> LL.OpDivF <$> desugarExp a <*> desugarExp b
  OpRemF a b -> LL.OpRemF <$> desugarExp a <*> desugarExp b
  
  OpAnd a b -> LL.OpAnd <$> desugarExp a <*> desugarExp b
  OpOr  a b -> LL.OpOr <$> desugarExp a <*> desugarExp b
  OpXor a b -> LL.OpXor <$> desugarExp a <*> desugarExp b

  OpEqI a b -> LL.OpEqI <$> desugarExp a <*> desugarExp b
  OpNeqI a b -> LL.OpNeqI <$> desugarExp a <*> desugarExp b

  OpLT a b -> LL.OpLT <$> desugarExp a <*> desugarExp b
  OpLE a b -> LL.OpLE <$> desugarExp a <*> desugarExp b
  OpGT a b -> LL.OpGT <$> desugarExp a <*> desugarExp b
  OpGE a b -> LL.OpGE <$> desugarExp a <*> desugarExp b

desugarType :: Type -> LL.Type
desugarType = \case
  ty@(TArr a b) ->
    let (paramtys, retty) = splitType ty
    in LL.TFunc (desugarType retty) (NE.fromList $ desugarType <$> paramtys) 

  TCon n  -> LL.TCon n
  TBool   -> LL.TBool
  TI8     -> LL.TI8
  TI32    -> LL.TI32
  TI64    -> LL.TI64
  TF32    -> LL.TF32
  TF64    -> LL.TF64
  TTuple t ts -> LL.TTuple (desugarType t) (desugarType <$> ts)
  TArray i ty -> LL.TArray i (desugarType ty)
  TPtr ty -> LL.TPtr (desugarType ty)
  TLoc t l -> LL.TLoc (desugarType t) l
  TParens t -> LL.TParens (desugarType t)
  

desugarPat :: Pat -> LL.Pat
desugarPat = \case
  PVar v      -> LL.PVar (name2String v)
  PCon n ps   -> LL.PCon n (desugarPat <$> ps)
  PTuple p ps -> LL.PTuple (desugarPat p) (desugarPat <$> ps)
  PWild       -> LL.PWild
  PType p ty  -> LL.PType (desugarPat p) (desugarType ty)
  PLoc p l    -> LL.PLoc (desugarPat p) l
  PParens p   -> LL.PParens (desugarPat p)



desugarClause :: MonadDesugar m => Clause -> m LL.Clause
desugarClause (Clause (Just l) bnd)
  = withLoc l $ desugarClause (Clause Nothing bnd)

desugarClause (Clause Nothing bnd) = do
  (p, e) <- unbind bnd
  let ns = (Just . fst) <$> patTypedVars p
  case exPAnn p of
    PCon n args -> do
      e' <- desugarExp e
      return $ LL.Clause n ns e'

    p -> do
      l <- ask
      error $ show $ vsep [ line <> pretty l <+> "error:"
                          , indent 4 $ vsep
                              [ "Simple pattern expected in case clause."
                              , "Found:" <+> pretty (show (exPAnn p))
                              , "in"
                              , pretty (show p) <+> "=" <+> pretty (show e)
                              ]
                          , line
                          ]


