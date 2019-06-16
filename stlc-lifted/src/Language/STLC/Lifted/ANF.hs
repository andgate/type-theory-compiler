{-# LANGUAGE LambdaCase
           , FlexibleContexts
           , RankNTypes
           , ConstraintKinds
           , TupleSections
           #-}
module Language.STLC.Lifted.ANF where


import Language.STLC.Lifted.Syntax
import qualified Language.ANF.Syntax as ANF

import Control.Monad
import Data.Maybe
import Data.List

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name (name2String, s2n)

-- This pass produces an ANF syntax tree by
-- lifting nested function applications into
-- let bindings.

-- This pass may require a heavy amount of local name generation.

-- Once ANF is produced, it can then be efficiently translated
-- to LLVM IR and then compiled to binary.

type Capture = (String, Exp)
type Captures = [Capture]


bindCaptures :: Fresh m => Captures -> ANF.Exp -> ANF.Type -> m ANF.Exp
bindCaptures caps e t1 = do
  caps' <- mapM (\(n, e@(EType _ ty)) -> (n,,) <$> anfType ty <*> anfExp e) caps
  let go e1 (n, t2, (e2, _)) = ANF.ELet (Just n) t2 e2 t1 e1
  return $ foldl' go e caps'

anfFunc :: Fresh m => Func -> m ANF.Func
anfFunc (Func fn_n ty bnd) = do
  let (paramtys, retty) = splitType ty
  (ns, body) <- unbind bnd
  paramtys' <- mapM anfType paramtys
  retty' <- anfType retty
  when (length paramtys' /= length ns)
       $ error $ "arity mismatch in function: " <> fn_n
  let params' = zip (name2String <$> ns)  paramtys'
  (body', caps) <- anfExp body
  body'' <- bindCaptures caps body' retty'
  return $ ANF.Func fn_n params' body'' retty'

anfExp  :: Fresh m => Exp -> m (ANF.Exp, Captures)
anfExp (EType e ty) = anfExp' ty e
anfExp _ = error "Expected typed expression!"


anfExp'  :: Fresh m => Type -> Exp -> m (ANF.Exp, Captures)
anfExp' ty = \case
  EVar n ->
    return (ANF.EVal . ANF.VVar . name2String $ n, [])
  
  EApp f@(EType _ f_ty) xs -> do
    let go (EType (EVar n) _) = do
            let n' = name2String n
            return $ (ANF.VVar n', [])
        go e = do
            n <- fresh (s2n "anf")
            let n' = name2String n
            return $ (ANF.VVar n', [(n', e)])
    
    r <- unzip <$> mapM go (f:xs)
    case r of
      ((ANF.VVar f'):xs', caps) -> do
          let e' = ANF.ECall f' xs'
          f_ty' <- anfType f_ty
          case f_ty' of
            ANF.TFunc retty' _ ->
              (, []) <$> bindCaptures (mconcat caps) e' retty' 
            
            _ -> error "expected function type as head of application"
      
      _ -> error "expected application head to be a var"
    
    
 
  
  ELet bnd -> do
    r_bnd <- unbind bnd
    case r_bnd of
      ((may_v, Embed rhs@(EType _ rhs_ty)), body@(EType _ body_ty)) -> do
        rhs_ty' <- anfType rhs_ty
        (rhs', rhs_caps) <- anfExp rhs
        rhs'' <- bindCaptures rhs_caps rhs' rhs_ty'

        body_ty' <- anfType body_ty
        (body', body_caps) <- anfExp body
        body'' <- bindCaptures body_caps body' body_ty'

        let e' = ANF.ELet (name2String <$> may_v) rhs_ty' rhs'' body_ty' body''
        return (e', [])
      
      _ -> error "anf: unable to unbind let!"


  EType e ty -> return $ error "Unexpected type annotation encountered"


anfVal :: Exp -> Maybe (ANF.Val)
anfVal = \case
  EVar n -> Just . ANF.VVar . name2String $ n
  EApp _ _ -> Nothing
  ELet _ -> Nothing
  EType e _ -> anfVal e
  EInt i -> Just $ ANF.VInt i

anfType :: Fresh m => Type -> m ANF.Type
anfType = \case
  ty@(TArr a b) ->
    let (paramtys, retty) = splitType ty
    in ANF.TFunc <$> anfType retty <*> mapM anfType paramtys 

  TCon n  -> pure $ ANF.TCon n
  TI8     -> pure ANF.TI8
  TI32    -> pure ANF.TI32
  TArray i ty -> ANF.TArray i <$> anfType ty
  TPtr ty -> ANF.TPtr <$> anfType ty
  TString -> pure ANF.TString
  TVoid   -> pure ANF.TVoid
  