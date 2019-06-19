{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Language.STLC.Lifted.Match where

-- Match generates simple patterns from nested patterns

import Control.Monad.Reader

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Language.STLC.Lifted.Syntax

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name (name2String, s2n)

type Equation = ([Pat], Exp)

type Constr = String
data Env = Env { envArity   :: Map Constr Int
               , envConstrs :: Map Constr [Constr]
               }
type MonadMatch m = (MonadReader Env m, Fresh m)


matchModule :: [Defn] -> [Defn]
matchModule modl = undefined


arity :: MonadMatch m => String -> m Int
arity c = reader (Map.lookup c . envArity) >>= maybe err return
  where err = error $ "Match: unrecognized constructor name: " ++ c

constrs :: MonadMatch m => String -> m [String]
constrs c = reader (Map.lookup c . envConstrs) >>= maybe err return
  where err = error $ "Match: unrecognized constructor name: " ++ c


isVar :: Equation -> Bool
isVar ((PVar _):_, _) = True
isVar ((PCon _ _):_, _)   = False
isVar ((PWild):_, _)  = True


isCon :: Equation -> Bool
isCon ((PVar _):_, _) = False
isCon ((PCon _ _):_, _)   = True
isCon ((PWild):_, _)  = False

getCon :: Equation -> String
getCon ((PCon n _):_, _) = n
getCon _ = error "expected constructor pattern!!"

partition :: Eq q => (a -> q) -> [a] -> [[a]]
partition f [] = []
partition f [x] = [[x]]
partition f (x:xs@(x':_))
  | f x == f x' = tack x (partition f xs)
  | otherwise   = [x]:(partition f xs)

tack :: x -> [[x]] -> [[x]]
tack x xss = (x : head xss) : tail xss


match :: MonadMatch m => [Var] -> [Equation] -> Exp ->  m Exp
match [] qs def = error "Expected variable list for match"
match us qs def = foldM (flip (matchVarCon us)) def (reverse $ partition isVar qs)


matchVarCon :: MonadMatch m => [Var] -> [Equation] -> Exp -> m Exp
matchVarCon us qs def
  | isVar (head qs) = matchVar us qs def
  | isCon (head qs) = matchCon us qs def

matchVar :: MonadMatch m => [Var] -> [Equation] -> Exp -> m Exp
matchVar (u:us) qs def
  = match us [(ps, subst v (EVar u) e) | (PVar v : ps, e) <- qs] def

matchCon :: MonadMatch m => [Var] -> [Equation] -> Exp -> m Exp
matchCon (u:us) qs def = do
  cs <- constrs (getCon (head qs))
  cs' <- mapM (\c -> matchClause c (u:us) (choose c qs) def) cs
  return $ ECase (EVar u) cs'


matchClause :: MonadMatch m => Constr -> [Var] -> [Equation] -> Exp -> m Clause
matchClause c (u:us) qs def = do
  k <- arity c
  us' <- mapM (\_ -> fresh (s2n "match.x")) [1..k] -- this is wrong, should be arity
  body <- match (us' ++ us) [(ps' ++ ps, e) | ((PCon n ps'):ps, e) <- qs] def
  return $ Clause (bind (PCon c (PVar <$> us')) body)

choose :: String -> [Equation] -> [Equation]
choose c qs = [q | q <- qs, getCon q == c]