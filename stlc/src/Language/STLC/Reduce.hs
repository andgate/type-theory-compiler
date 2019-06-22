{-# LANGUAGE LambdaCase
           , ConstraintKinds
           , FlexibleContexts
           #-}
module Language.STLC.Reduce where

import Language.STLC.Syntax

import Control.Monad.Reader

import Data.Bifunctor
import Data.Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name (name2String, s2n)

type Env = Map String Exp

type MonadReduce m = (MonadReader Env m, Fresh m)


lookupVar ::MonadReader Env m => Var -> m (Maybe Exp)
lookupVar v = reader $ Map.lookup (name2String v)

reduceBy :: Env -> Int -> Exp -> Exp
reduceBy env i e = either id id $ runFreshM $ runReaderT (reduceBy' i e) env

reduceBy' :: MonadReduce m => Int -> Exp -> m (Either Exp Exp)
reduceBy' 0 e = return $ Left e
reduceBy' fuel e = do
  some_e' <- reduce e
  case some_e' of
    Left e' -> return $ Left e'
    Right e' -> reduceBy' (fuel-1) e'


reduce :: MonadReduce m => Exp -> m (Either Exp Exp)
reduce = \case
  EVar v -> do
    may_e <- lookupVar v
    case may_e of
      Just e -> return $ Right e
      Nothing -> return $ Left $ EVar v

  ELit l -> bimap ELit ELit <$> reduceLit l
  
  EType e ty -> bimap (`EType` ty) (`EType` ty) <$> reduce e

  EApp f xs -> do
    some_xs' <- mapM reduce xs
    let xs' = either id id <$> some_xs'
    if length (rights some_xs') /= 0
      then return $ Right $ EApp f xs'
      else do
        some_f' <- reduce f
        case some_f' of
          Left (ELam bnd) -> do 
            (ps, body) <- unbind bnd
            let qs' = concatMap extractPat (zip ps xs')
                body' = substs qs' body
            return $ Right body'

            
          Left f' -> error $ "reduce - can't apply non-function:\n\n" ++ show f' ++ "\n\n"
          Right f' -> return $ Right $ EApp f' xs'


  ELam bnd -> return $ Left $ ELam bnd

  ELet bnd -> return $ Left $ ELet bnd
  EIf p t f -> return $ Left $ EIf p t f

  ECase e cls -> return $ Left $ ECase e cls

  ERef e -> return $ Left $ ERef e
  EDeref e -> return $ Left $ EDeref e
  
  ECon n es -> do
    some_es' <- mapM reduce es
    let es' = either id id <$> some_es'
    if length (rights some_es') == 0
      then return $ Left $ ECon n es'
      else return $ Right $ ECon n es'

  ENewCon n es -> return $ Left $ ENewCon n es
  EFree e -> return $ Left $ EFree e

  EGet e m -> return $ Left $ EGet e m
  EGetI e i -> return $ Left $ EGetI e i
  ESet lhs rhs -> return $ Left $ ESet lhs rhs

  ENewArray e -> return $ Left $ ENewArray e
  ENewArrayI e -> return $ Left $ ENewArrayI e
  EResizeArray e s -> return $ Left $ EResizeArray e s

  ENewString s -> return $ Left $ ENewString s
  ENewStringI e -> return $ Left $ ENewStringI e

  EOp op -> reduceOp op


reduceLit :: MonadReduce m => Lit -> m (Either Lit Lit)
reduceLit = \case
  LInt i -> return $ Left $ LInt i
  LChar c -> return $ Left $ LChar c

  LString str -> return $ Left $ LString str

  LStringI e -> bimap LStringI LStringI <$> reduce e

  LArray xs -> do
    xs' <- mapM reduce xs
    if length (rights xs') == 0
      then return $ Left $ LArray (lefts xs')
      else return $ Right $ LArray (either id id <$> xs')

  LArrayI e -> do
    bimap LArrayI LArrayI <$> reduce e


extractPat :: (Pat, Exp) -> [(Var, Exp)]
extractPat (pat, e)
  = case pat of
      PVar v -> [(v, e)]
      PCon n ps ->
        case e of
          EType (ECon con_n es) _ 
            | length ps == length es -> concatMap extractPat (zip ps es)
            | otherwise -> error $ "extractPat - Pattern-Constructor length mismatch!"
          
          _ -> error $ "Unsupported pattern match.\n\n"
                    ++ "Pattern: " ++ show (PCon n ps) ++ "\n\n" 
                    ++ "Expression: " ++ show e ++ "\n\n"

      PWild -> []
      PType p _ -> extractPat (p, e)


reduceOp :: MonadReduce m => Op -> m (Either Exp Exp)
reduceOp = \case
  OpAddI a b -> reduceBinaryOpI OpAddI (+) a b
  OpSubI a b -> reduceBinaryOpI OpAddI (-) a b
  OpMulI a b -> reduceBinaryOpI OpAddI (*) a b

  _ -> error $ "Unsupported operator encountered!"


reduceBinaryOpI :: MonadReduce m
               => (Exp -> Exp -> Op) -> (Int -> Int -> Int)
               -> Exp -> Exp -> m (Either Exp Exp)
reduceBinaryOpI constr instr a b = do
  some_a' <- reduce a
  case some_a' of
    Right a' -> return $ Right $ EOp $ constr a' b
    Left a' -> do
      some_b' <- reduce b
      case some_b' of
        Right b' -> return $ Right $ EOp $ constr a' b'
        Left b' ->
          case (a', b') of
            (ELit (LInt x), ELit (LInt y)) ->
              return $ Left $ ELit (LInt (instr x y))
            
            _ -> return $ Left $ EOp $ constr a' b'