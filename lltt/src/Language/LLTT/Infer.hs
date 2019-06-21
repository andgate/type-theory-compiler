module Language.LLTT.Infer where

import Language.LLTT.Syntax
{-# LANGUAGE LambdaCase
           , ConstraintKinds
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , ViewPatterns
           , DeriveGeneric
           , DeriveDataTypeable
           , TypeSynonymInstances
          #-}

{-
-- Type Inference
--   Type inference will enrich the ast with type annotations.
-- While this language is intended to be the target of a
-- higher level language, type inference is necessary
-- for generating tests.

import Language.STLC.Lifted.Syntax

import Control.Monad.Reader

import Data.Bifunctor
import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless


-----------------------------------------------------------------
-- Environment and Inference Monad
-----------------------------------------------------------------

type Substitution = (PolyType, PolyType)
type Substitutions = [Substitution]
type Constraint   = (PolyType, PolyType)
type Constraints = [Constraint]

type Env = Map String Type
type Infer = ReaderT Env FreshM
type MonadInfer m = (MonadReader Env m, Fresh m)

-- helpers

lookupType :: MonadReader Env m => String -> m Type
lookupType n
  = reader (Map.lookup n) >>= maybe err return
  where err = error $ "Check: untyped variable encounted: " ++ n

withType :: MonadReader Env m => String -> Type -> m a -> m a
withType n ty
  = local (Map.insert n ty)

withTypes :: MonadReader Env m => [(String, Type)] -> m a -> m a
withTypes ns = local (\env -> foldl f env ns)
  where f env (n, ty) = Map.insert n ty env 

inferModule :: Module -> Module
inferModule (Module n defns)
  = Module n defns'
  where env = makeEnv defns
        m = mapM inferDefn defns
        defns' = runFreshM (runReaderT m env)

makeEnv :: [Defn] -> Map String Type
makeEnv defns = Map.fromList defns''
  where defns' = concatMap f defns
        defns'' = nubBy (\(n1,_) (n2, _) -> if n1 == n2 then err n1 else False) defns'
        err n = error $ "Illegal duplicate definition: " ++ n
        f = \case
          FuncDefn (Func ty f bnd) -> [(f, ty)]
          
          ExternDefn (Extern n paramty retty) ->
            [(n, tarr paramty retty)]

          DataTypeDefn (DataType dt_n dt_cons) ->
            let mems = mconcat (snd <$> dt_cons)
                mem_tys = [ (n, TArr (TCon dt_n) ty) | (Just n, ty) <- mems ]
                con_tys = do
                  (con_n, con_params) <- dt_cons
                  let v = con_n
                      ty = tarr (snd <$> con_params) (TCon dt_n)
                  return (v, ty)
            in mem_tys ++ con_tys


-----------------------------------------------------------
-- Algorithm W
--   This algorithm does type inference in three phases:
--      1) Constraint gathering, temporary type variable instantiation
--      2) Unification, turning constraints into substitutions
--      3) Inference, apply substitutions and
--         convert back to a monotype.
-----------------------------------------------------------

-----------------------------------------------------------
-- Constraint Gathering
-----------------------------------------------------------


constraints :: Fresh m => Exp -> m (PolyExp, Constraints)
constraints = \case
  EVar v -> do
    ty <- lookupType (name2String v)
    return (EType (EVar v) ty, [])

  ELit l -> do
    ty <- typeOfLit
    return $ (EType (ELit l) ty, [])

  EType e ty -> do
    (e'@(EType _ ty'), cs) <- constraints e
    return (e', (ty, ty'):cs)

  EApp f xs -> do
    (f, cs1) <- constraints f
    (xs', css) <- unzip <$> mapM constraints xs
    let cs2 = mconcat css
        (paramtys, retty) = splitType $ exType f
        paramtys' = exType <$> xs'
        cs3 = zip paramtys paramtys'
        e' = EType (EApp f' xs') retty 
    return (e', cs1 <> cs2 <> cs3)

  ELet _ -> undefined
  EIf _ _ _ -> undefined
  ECase _ _ -> undefined

  ERef _ -> undefined
  EDeref _ -> undefined
  
  ECon _ [] -> undefined
  ECon _ _ -> undefined
  ENewCon _ _ -> undefined
  EFree e -> undefined
  
  EGet e _ -> isAExp e
  EGetI e _ -> isAExp e
  ESet _ _ -> undefined

  ENewArray _ -> undefined
  ENewArrayI _ -> undefined
  EResizeArray _ _ -> undefined

  ENewString _ -> undefined
  ENewStringI _ -> undefined
  
  EOp _ -> undefined


typeOfLit :: Fresh m => Lit -> m Type
typeOfLit = \case
  LInt _ -> newTVar
  LChar _ -> return TChar
  LString _ -> return TString
  LStringI _ -> return TString
  LArray _ -> newTVar
  LArrayI _ -> newTVar


inferDefn :: MonadInfer m => Defn -> m Defn
inferDefn = \case
  FuncDefn (Func ty f bnd) -> do
    (ps, body) <- unbind bnd

    let (argtys, retty) = splitType ty
    ps' <- mapM (uncurry inferPat) (zip argtys ps)
    let paramtys = concatMap patTypedVars ps' 
    body' <- withTypes paramtys $ infer body

    let bnd' = bind ps' body'
    return $ FuncDefn $ Func ty f bnd'

  defn -> return defn


infer :: MonadInfer m => Exp -> m Exp
infer = \case
  e@(EVar v) -> EType e <$> lookupType (name2String v)

  ELit l -> undefined

  EType e t -> do
    e' <- infer e
    case e' of
      EType _ t'
        | t /= t' -> error "Type mismatch!"
        | True    -> return e'

      _ -> error "Type inference has mysteriously failed!"

  EApp f xs -> do
    f' <- infer f
    xs' <- mapM infer xs
    let f_ty = exType f'
        (paramtys, retty) = splitType f_ty
        x_tys = exType <$> xs'
    
    if and (zipWith (==) x_tys paramtys)
      then
        return $ EType (EApp f' xs') retty
      else error $ "tcabalype application mismatch\n\n"
                ++ "expected: " ++ show paramtys ++ "\n\n"
                ++ "actual: " ++ show x_tys ++ "\n\n"

  ELet bnd -> do
    (unrec -> letbnds, body) <- unbind bnd
    let qs = second unembed <$> letbnds
        go (ns, qs) (p, e) = withTypes ns $ do
          e' <- infer e
          case e' of
            EType _ ty -> do
              p' <- inferPat ty p
              let ns' = patTypedVars p'
              return (ns' ++ ns, (p', e'):qs)
            _ -> error "Inference: Expected typed expression in let equation"
    (ns, qs') <- foldM go ([], []) qs
    withTypes ns $ do
      body' <- infer body
      let letbnds' = rec (second embed <$> qs')
      return $ EType (ELet $ bind letbnds' body')
                     (exType body')

  ECase e cls -> do
    e' <- infer e
    case e' of
      EType _ ty -> do
        cls' <- mapM (inferClause ty) cls
        case cls' of
          (Clause bnd):_ -> do
            (cls_p, cls_body) <- unbind bnd
            case cls_body of
              EType _ cls_ty ->
                return $ EType (ECase e' cls') cls_ty
              _ -> error "Expected clause type!"
          _ -> error "Empty case encountered!"

      _ -> error "Expected typed expression!"

  ECon n args -> do
    (paramtys, retty) <- splitType <$> lookupType n
    let argtys = zipWith EType args paramtys
    e' <- ECon n <$> mapM infer argtys
    return $ EType e' retty

  ENewCon n args -> do
    (paramtys, retty) <- splitType <$> lookupType n
    let argtys = zipWith EType args paramtys
    e' <- ENewCon n <$> mapM infer argtys
    return $ EType e' retty

  EFree e ->
    EType <$> (EFree <$> infer e) <*> pure TVoid

  EDeref e -> do
    e' <- infer e
    case e' of
      EType _ (TPtr ty) -> return $ EType (EDeref e') ty
      _ -> error "Type check error: can't dereference a non-pointer"

  ERef e -> do
    e' <- infer e
    case e' of
      EType _ ty -> return $ EType (ERef e') (TPtr ty)
      _ -> error "Type check error: can't dereference a non-pointer"

  EGet e mem_n -> do
    e' <- infer e
    ty <- lookupType mem_n
    let (argtys', retty') = splitType ty
    case e' of
      EType _ argty ->
        case argtys' of
          [argty'] 
            | argty' == argty -> return $ EType (EGet e' mem_n) retty'
            | otherwise -> error $ "Type mismatch!"
          _ -> error $ "Expected member to only take one argument: " ++ mem_n
      _ -> error "Expected typed expression"

  EOp op -> inferOp op

inferClause :: MonadInfer m => Type -> Clause -> m Clause
inferClause ty (Clause bnd) = do
  (p, e) <- unbind bnd
  p' <- inferPat ty p
  let ns = patTypedVars p'
  withTypes ns $ do
    e' <- infer e
    let bnd' = bind p' e'
    return $ Clause bnd'


inferOp :: MonadInfer m => Op -> m Exp
inferOp = \case
  OpAddI a b -> do
    op' <- OpAddI <$> infer (EType a TI32) <*> infer (EType b TI32)
    return $ EType (EOp op') TI32

  OpMulI a b -> do
    op' <- OpMulI <$> infer (EType a TI32) <*> infer (EType b TI32)
    return $ EType (EOp op') TI32


inferPat :: MonadInfer m => Type -> Pat -> m Pat
inferPat ty = \case
  PVar v -> return $ PType (PVar v) ty

  PCon n ps -> do
    n_ty <- lookupType n
    let (argtys, retty) = splitType n_ty
    ps' <- mapM (\(p, ty') -> inferPat ty' p) (zip ps argtys)
    if ty /= retty
      then error $ "Pattern type mismatch"
      else return $ PType (PCon n ps') retty

  PWild -> return $ PType PWild ty

  PType p ty' 
    | ty /= ty' -> do
        env <- ask
        error $ "\nPattern type mismatch:\n"
            ++ "pattern: " ++ show p ++ "\n\n"
            ++ "actual type: " ++ show ty ++ "\n\n"
            ++ "expected type: " ++ show ty' ++ "\n\n"
            ++ "env: " ++ show env ++ "\n\n"
    | True -> inferPat ty' p
-}
-----------------------------------------------------------
-- Unification
-----------------------------------------------------------

--unify :: Constraints -> Substitutions
--unify = undefined


-----------------------------------------------------------
-- Inference
-----------------------------------------------------------

--infer :: Substitutions -> PolyExp -> Exp