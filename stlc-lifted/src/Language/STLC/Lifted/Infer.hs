{-# LANGUAGE LambdaCase
           , ConstraintKinds
           , FlexibleContexts
           , ViewPatterns
          #-}
module Language.STLC.Lifted.Infer where

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

import Unbound.Generics.LocallyNameless


type Env = Map String Type
type Infer = ReaderT Env FreshM
type MonadInfer m = (MonadReader Env m, Fresh m)

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

inferModule :: [Defn] -> [Defn]
inferModule modl = runFreshM (runReaderT m env)
  where env = makeEnv modl
        m = mapM inferDefn modl

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
      else error $ "type application mismatch\n\n"
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
  
  e@(EInt _) -> return $ EType e TI32
  e@(EString _) -> return $ EType e TString

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

  EMember e mem_n -> do
    e' <- infer e
    ty <- lookupType mem_n
    let (argtys', retty') = splitType ty
    case e' of
      EType _ argty ->
        case argtys' of
          [argty'] 
            | argty' == argty -> return $ EType (EMember e' mem_n) retty'
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
