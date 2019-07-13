{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , LambdaCase
           , ViewPatterns
           , TupleSections
           #-}
module Language.STLC.Match where

-- Match generates simple patterns from nested patterns

import Control.Monad.Reader

import Data.Bifunctor

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Language.Syntax.Location
import Language.STLC.Syntax

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name (name2String, s2n)

type Equation = ([Pat], Exp)

type Constr = String
data Env = Env { envArity   :: Map Constr Int
               , envConstrs :: Map Constr [Constr]
               , envConstrTys :: Map Constr Type
               , envLoc :: Loc
               }
type MonadMatch m = (MonadReader Env m, Fresh m)

envUpdateLoc :: Loc -> Env -> Env
envUpdateLoc l env = env { envLoc = l }

withLoc :: MonadMatch m => Loc -> m a -> m a
withLoc l = local (envUpdateLoc l)


matchModule :: Module -> Module
matchModule (Module l n defns)
  = Module l n defns'
  where env = makeEnv l defns
        defns' = runFreshM $ runReaderT (mapM matchDefn defns) env


makeEnv :: Loc -> [Defn] -> Env
makeEnv l modl = Env { envArity = Map.fromList rs
                     , envConstrs = Map.fromList cs
                     , envConstrTys = Map.fromList contys
                     , envLoc = l
                     }
  where (rs, cs, contys) = foldl (\(rs, cs, contys) (rs', cs', contys') ->
                                      (rs ++ rs', cs ++ cs', contys ++ contys') )
                                 ([],[], [])
                                 $ map go modl

        -- Extract environment information from datatype definitions
        go :: Defn 
           -> ( [(Constr, Int)]      -- ^ Constructor name to constructor arity
              , [(Constr, [Constr])] -- ^ Constructor name to sibling constructor names
              , [(Constr, Type)])    -- ^ Constructor name to type
        go = \case
          DataTypeDefn (DataType _ dt_n dt_cons) ->
            let cons = constrName <$> dt_cons
                rs = second constrArity <$> zip cons dt_cons  
                cs = (,cons) <$> cons                     
                contys = zip cons (repeat $ TCon dt_n)    
            in (rs, cs, contys)

          defn ->
            ([], [], [])

matchDefn :: MonadMatch m => Defn -> m Defn
matchDefn = \case
  FuncDefn f -> FuncDefn <$> matchFunc f
  defn -> pure defn

matchFunc :: MonadMatch m => Func -> m Func
matchFunc (Func l ty f bnd) = withLoc l $ do
  (ps, body) <- unbind bnd
  body' <- matchExp body
  let bnd' = bind ps body'
  return $ Func l ty f bnd'

matchExp :: MonadMatch m => Exp -> m Exp
matchExp exp = case exp of
  EVar v -> return exp
  ELit _ -> return exp
  EType e ty -> EType <$> matchExp e <*> pure ty
  EApp f xs -> EApp <$> matchExp f <*> (mapM matchExp xs)

  ELet bnd -> do
    (unrec -> qs, body) <- unbind bnd
    es' <- mapM (matchExp . unembed . snd) qs
    let qs' = NE.zip (fst <$> qs) (embed <$> es')
    body' <- matchExp body
    let bnd' = bind (rec qs') body'
    return $ ELet bnd'

  EIf p t f ->
    EIf <$> matchExp p <*> matchExp t <*> matchElse f

  ECase e@(EType _ ty) clauses -> do
    v <- fresh (s2n "match.e")
    e' <- matchExp e
    qs' <- forM clauses $ \(Clause _ bnd) ->
            do  (p, body) <- unbind bnd
                body' <- matchExp body
                return ([p], e')

    body <- match [v] (NE.toList qs')
                  (eapp "error" [ELit $ LString "Default match"])
    return $ elet [(PType (PVar v) ty, e')] body

  ERef e -> ERef <$> matchExp e
  EDeref e -> EDeref <$> matchExp e
  
  ECon n xs -> ECon n <$> mapM matchExp xs
  ENewCon n xs -> ENewCon n <$> mapM matchExp xs
  EFree e -> EFree <$> matchExp e
  
  EGet e mem_n -> EGet <$> matchExp e <*> pure mem_n
  EGetI e i -> EGetI <$> matchExp e <*> matchExp i
  ESet lhs rhs -> ESet <$> matchExp lhs <*> matchExp rhs
  
  ENewArray xs -> ENewArray <$> mapM matchExp xs
  ENewArrayI i -> ENewArrayI <$> matchExp i
  EResizeArray e i -> EResizeArray <$> matchExp e <*> matchExp i

  ENewString _ -> return exp
  ENewStringI i -> ENewStringI <$> matchExp i

  EOp op -> EOp <$> matchOp op
  ELoc e l -> ELoc <$> withLoc l (matchExp e) <*> pure l

matchElse :: MonadMatch m => Else -> m Else
matchElse = \case
  Elif Nothing p t f
    -> Elif Nothing <$> matchExp p <*> matchExp t <*> matchElse f
  Elif (Just l) p t f
    -> withLoc l $ Elif (Just l) <$> matchExp p <*> matchExp t <*> matchElse f
  
  Else Nothing e -> Else Nothing <$> matchExp e
  Else (Just l) e -> withLoc l $ Else (Just l) <$> matchExp e

matchOp :: MonadMatch m => Op -> m Op
matchOp = \case
  OpAdd a b -> OpAdd <$> matchExp a <*> matchExp b
  OpSub a b -> OpSub <$> matchExp a <*> matchExp b
  OpMul a b -> OpMul <$> matchExp a <*> matchExp b



arity :: MonadMatch m => String -> m Int
arity c = reader (Map.lookup c . envArity) >>= maybe err return
  where err = error $ "Match: unrecognized constructor name: " ++ c

constrs :: MonadMatch m => String -> m [String]
constrs c = reader (Map.lookup c . envConstrs) >>= maybe err return
  where err = error $ "Match: unrecognized constructor name: " ++ c


constrTys :: MonadMatch m => String -> m Type
constrTys c = reader (Map.lookup c . envConstrTys) >>= maybe err return
  where err = error $ "Match: unrecognized constructor name: " ++ c


isVar :: Equation -> Bool
isVar ((PVar _):_, _) = True
isVar ((PCon _ _):_, _)   = False
isVar ((PWild):_, _)  = True
isVar ((PType p _):ps, e) = isVar (p:ps, e)


isCon :: Equation -> Bool
isCon ((PVar _):_, _) = False
isCon ((PCon _ _):_, _)   = True
isCon ((PWild):_, _)  = False
isCon ((PType p _):ps, e) = isCon (p:ps, e)

getCon :: Equation -> String
getCon ((PCon n _):_, _) = n
getCon ((PType p _):ps, e) = getCon (p:ps, e)
getCon _ = error "expected constructor pattern!!"

getEqType :: Equation -> Type
getEqType (_, EType _ ty) = ty
getEqType _ = error "Expected equation type!!"

getEqPatType :: Equation -> Type
getEqPatType ((PType _ ty):_, _) = ty
getEqPatType eq = error $ "getEqPatType - Expected typed pattern:\n\n" ++ show eq ++ "\n\n"

partition :: Eq q => (a -> q) -> [a] -> [[a]]
partition f [] = []
partition f [x] = [[x]]
partition f (x:xs@(x':_))
  | f x == f x' = tack x (partition f xs)
  | otherwise   = [x]:(partition f xs)

tack :: x -> [[x]] -> [[x]]
tack x xss = (x : head xss) : tail xss


match :: MonadMatch m => [Var] -> [Equation] -> Exp -> m Exp
match [] ((_, e):_) _ = pure e 
match us qs def
  = foldM (flip $ matchVarCon us) def (reverse $ partition isVar qs)


matchVarCon :: MonadMatch m => [Var] -> [Equation] -> Exp -> m Exp
matchVarCon us qs def
  | isVar (head qs) = matchVar us qs def
  | isCon (head qs) = matchCon us qs def

matchVar :: MonadMatch m => [Var] -> [Equation] -> Exp -> m Exp
matchVar (u:us) qs def
  = match us [(ps, subst v (EVar u) e) | (PType (PVar v) _ : ps, e) <- qs] def

matchCon :: MonadMatch m => [Var] -> [Equation] -> Exp -> m Exp
matchCon (u:us) qs def = do
  cs <- constrs (getCon (head qs))
  cs' <- mapM (\c -> matchClause c (u:us) (choose c qs) def) cs
  let ty = getEqType (head qs)
      uty = getEqPatType (head qs)
  l <- reader envLoc
  return $ ELoc (EType (ECase (EType (EVar u) uty) (NE.fromList cs')) ty) l


matchClause :: MonadMatch m => Constr -> [Var] -> [Equation] -> Exp -> m Clause
matchClause c (u:us) qs def = do
  let ty = getEqPatType (head qs)
  conty <- constrTys c
  k <- arity c
  us' <- mapM (\_ -> fresh (s2n "match.x")) [1..k]
  body <- match (us' ++ us) [(ps' ++ ps, e) | ((PType (PCon n ps') _):ps, e) <- qs] def
  l <- reader envLoc
  return $ Clause (Just l) (bind (PType (PCon c (PType <$> (PVar <$> us') <*> pure ty)) conty)  body)

choose :: String -> [Equation] -> [Equation]
choose c qs = [q | q <- qs, getCon q == c]