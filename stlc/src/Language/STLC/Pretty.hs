{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , FlexibleInstances
           , ViewPatterns
           #-}
module Language.STLC.Pretty where

import Language.STLC.Syntax
import qualified Data.List.NonEmpty as NE
import Unbound.Generics.LocallyNameless
import Data.Text.Prettyprint.Doc


instance Pretty Module where
  pretty (Module _ n ds)
    = vsep (("module" <+> pretty n <> line):(pretty <$> ds))


instance Pretty Defn where
  pretty = \case
    FuncDefn f -> pretty (FreshP f) <> line
    ExternDefn ex -> pretty ex <> line
    DataTypeDefn dt -> pretty dt <> line


instance Pretty Extern where
  pretty (Extern _ n paramtys retty)
    = hsep ["extern", pretty n, ":", pretty (tarr paramtys retty)]


instance Pretty DataType where
  pretty (DataType _ n [])
    = "type" <+> pretty n

  pretty (DataType _ n (c:[]))
    = "type" <+> pretty n <+> "=" <+> pretty c

  pretty (DataType _ n (c:cs))
    = vsep [ "type" <+> pretty n
           , indent 2 $ vsep $
              [ "=" <+> pretty c ] 
               ++ ["|" <+> pretty c' | c' <- cs]
           ]

instance Pretty ConstrDefn where
  pretty = \case
    ConstrDefn _ n tys -> pretty n <+> hsep (pretty <$> tys)
    RecordDefn _ n (NE.toList -> ens) -> pretty n <+> encloseSep lbrace rbrace comma (pretty <$> ens)

instance Pretty Entry where
  pretty (Entry _ n ty) = pretty n <+> ":" <+> pretty ty


instance Pretty Type where
  pretty = \case
    TArr a b -> pretty a <+> "->" <+> pretty b
    TCon n -> pretty n
    TInt  i -> "I" <> pretty i
    TUInt i -> "U" <> pretty i
    TFp   i -> "F" <> pretty i
    TTuple t (NE.toList -> ts)
      -> tupled $ pretty <$> (t:ts)
    
    TArray i ty 
      | isAType ty -> pretty ty <> brackets (pretty i)
      | True       -> parens (pretty ty) <> brackets (pretty i)
    
    TVect i ty 
      | isAType ty -> pretty ty <> brackets (pretty i)
      | True       -> parens (pretty ty) <> angles (pretty i)
      
    TPtr ty 
      | isAType ty -> "*" <> pretty ty
      | True       -> "*" <> parens (pretty ty)
    TLoc ty _ -> pretty ty
    TParens ty -> parens $ pretty ty

instance Pretty Exp where
  pretty = pretty . FreshP

class PrettyFresh a where
  prettyFresh :: Fresh m => a -> m (Doc ann)

newtype FreshP a = FreshP a 

instance PrettyFresh a => Pretty (FreshP a) where
  pretty (FreshP a) = runFreshM $ prettyFresh a


instance PrettyFresh Func where
  prettyFresh (Func _ ty n bnd) = do
    (ps, body) <- unbind bnd
    let ps' = do
          p <- ps
          let p' = pretty p
          return $ if isAPat p then p' else parens p'
    body' <- prettyFresh body
    case ps' of
      [] -> if isAExp body || isBExp body
              then return $ vsep [ pretty n <+> ":" <+> pretty ty
                                , pretty n <+> "=" <+> body'
                                ]
              else return $ vsep [ pretty n <+> ":" <+> pretty ty
                                  , pretty n <+> "="
                                  , indent 2 body'
                                  ]

      _ -> if isAExp body || isBExp body
              then return $ vsep [ pretty n <+> ":" <+> pretty ty
                                , pretty n <+> hsep ps' <+> "=" <+> body'
                                ]
              else return $ vsep [ pretty n <+> ":" <+> pretty ty
                                  , pretty n <+> hsep ps'
                                  , indent 2 ("=" <+> align body')
                                  ]

instance PrettyFresh Exp where
  prettyFresh = \case
    EVar n -> return $ pretty (name2String n)

    ELit l -> prettyFresh l
    EApp f (NE.toList -> xs) -> do
      f' <- wrapBExpFresh f
      xs' <- mapM wrapBExpFresh xs
      return $ hsep (f':xs') 

    
    EType e ty -> do
      e' <- prettyFresh e
      let ty' = pretty ty
      if isAExp e || isBExp e 
        then return $ hsep [e', ":", ty']
        else return $ hsep [parens e', ":", ty']
    
    ECast e ty -> do
      e' <- prettyFresh e
      let ty' = pretty ty
      if isAExp e || isBExp e 
        then return $ hsep [e', "as", ty']
        else return $ hsep [parens e', "as", ty']
    
    ELoc e _ -> prettyFresh e
    EParens e -> parens <$> prettyFresh e

    ELam bnd -> do
      (NE.toList -> ps, body) <- unbind bnd
      body' <- prettyFresh body
      return $ align $ vsep
        [ "\\" <+> hsep (pretty <$> ps) <+> "->"
        , indent 4 body'
        ]
      
    
    ELet bnd -> do
      (r_qs, body) <- unbind bnd
      let (ps, es) = unzip [ (p, e) | (p, Embed e) <- NE.toList (unrec r_qs)]
      let ps' = pretty <$> ps
      es' <- mapM prettyFresh es
      let qs' = [ q' <+> "=" <+> e'
                | (q', e') <- zip ps' es' ]
      body' <- prettyFresh body
      return $ vsep [ "let" <+> align (vsep qs')
                    , "in" <+> body'
                    ]
    
    EIf p t f -> do
      p' <- prettyFresh p
      t' <- prettyFresh t
      f' <- prettyFresh f
      return $ vsep [ "if" <+> p'
                    , indent 2 ("then" <+> t')
                    , indent 2 f'
                    ]

    ECase e cls -> do
      e' <- prettyFresh e
      cls' <- mapM prettyFresh (NE.toList cls)
      return $ vsep $ [ "case" <+> e' <+> "of"
                      , indent 2 (vsep cls')]
    
    ERef e -> do
      e' <- prettyFresh e
      if isAExp e
        then return $ "&" <> e'
        else return $ "&" <> parens e'

    EDeref e -> do
      e' <- prettyFresh e
      if isAExp e
        then return $ "*" <> e'
        else return $ "*" <> parens e'
    
    ETuple e (NE.toList -> es) ->
      tupled <$> mapM prettyFresh (e:es)

    ECon n [] ->
      return $ pretty n

    ECon n args -> do
      args' <- mapM prettyFresh args
      return $ pretty n <+> hsep args'

    ENewCon n [] ->
      return $ "new" <+> pretty n

    ENewCon n args -> do
      args' <- mapM prettyFresh args
      return $ "new" <+> pretty n <+> hsep args'

    EFree e -> do
      e' <- prettyFresh e
      return $ "free" <+> e'

    EGet e m -> do
      e' <- wrapBExpFresh e
      return $ e' <> "." <> pretty m


    EGetI e i -> do
      e' <- wrapBExpFresh e
      i' <- prettyFresh i
      return $ e' <> brackets i'

    ESet rhs lhs -> do
      lhs' <- prettyFresh lhs
      rhs' <- prettyFresh rhs
      return $ lhs' <+> "<-" <> rhs'

    ENewArray xs -> do
      xs' <- mapM prettyFresh xs
      return $ "new" <+> list xs'

    ENewArrayI i -> do
      i' <- prettyFresh i
      return $ "new" <+> "Array" <> brackets i'
    
    EResizeArray e i -> do
      e' <- wrapBExpFresh e
      i' <- prettyFresh i
      return $ "resize" <+> e' <> brackets i'

    ENewVect xs -> do
      xs' <- mapM prettyFresh xs
      return $ "new" <+> encloseSep "<" ">" "," xs'

    ENewVectI i -> do
      i' <- prettyFresh i
      return $ "new" <+> "Vect" <> angles i'

    ENewString str ->
      return $ "new" <+> dquotes (pretty str)

    EOp op -> prettyFresh op


instance PrettyFresh Lit where
  prettyFresh = \case
    LNull -> return "null"
    LBool b -> return $ pretty b
    LInt i -> return $ pretty i
    LDouble d -> return $ pretty d
    LChar c -> return $ squotes $ pretty c
    LString s -> return $ dquotes $ pretty s

    LArray es -> do
      es' <- mapM prettyFresh es
      return $ list es'

    LArrayI i -> do
      i' <- prettyFresh i
      return $ "Array" <> brackets i'

    LVect es -> do
      es' <- mapM prettyFresh es
      return $ encloseSep "<" ">" "," es'

    LVectI i -> do
      i' <- prettyFresh i
      return $ "Vect" <> angles i'

instance PrettyFresh Else where
  prettyFresh = \case
    Elif _ p t f -> do
      p' <- prettyFresh p
      t' <- prettyFresh t
      f' <- prettyFresh f
      return $ vsep [ "elif" <+> p'
                    , "then" <+> t'
                    , f'
                    ]

    Else _ body -> do
      body' <- prettyFresh body
      return $ "else" <+> body'

instance Pretty Pat where
  pretty = \case
    PVar v -> pretty $ name2String v
    PCon n [] -> pretty n
    PCon n ps ->
      let ps' = do
            p <- ps
            return $ if isAPat p
                      then pretty p
                      else parens $ pretty p
      in pretty n <+> hsep ps'
  
    PTuple p (NE.toList -> ps) ->
      tupled $ pretty <$> (p:ps)
    PWild -> "_"
    PType p ty -> hsep [pretty p, ":", pretty ty]
    PLoc p _ -> pretty p
    PParens p -> parens $ pretty p

instance PrettyFresh Clause where
  prettyFresh (Clause _ bnd) = do
    (p, e) <- unbind bnd
    let p' = if isAPat p || isBPat p
               then pretty p
               else parens $ pretty p                     
    e' <- prettyFresh e
    if isAExp e || isBExp e
      then
        return $ p' <+> "->" <+> e'
      else
        return $ vcat [ p' <+> "->"
                      , indent 2 e'
                      , mempty
                      ]

instance PrettyFresh Op where
  prettyFresh = \case
    OpAdd a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#add", a', b']

    OpSub a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#sub", a', b']

    OpMul a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#mul", a', b']
    
    OpDiv a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#div", a', b']
    
    OpRem a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#rem", a', b']

    OpNeg a  -> do
      a' <- wrapBExpFresh a
      return $ hsep ["#neg", a']


    OpAnd a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#and", a', b']

    OpOr a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#or", a', b']

    OpXor a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#xor", a', b']

    OpShR a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#shr", a', b']

    OpShL a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#shl", a', b']

    OpEq a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#eq", a', b']

    OpNeq a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#neq", a', b']


    OpLT a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#lt", a', b']

    OpLE a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#le", a', b']

    OpGT a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#gt", a', b']

    OpGE a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["#ge", a', b']


-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

wrapBExpFresh :: Fresh m => Exp -> m (Doc ann)
wrapBExpFresh e 
  | isAExp e = prettyFresh e
  | True     = parens <$> prettyFresh e


isBExp :: Exp -> Bool
isBExp = \case
  EVar _ -> False
  ELit _ -> False
  EApp _ _ -> True

  EType _ _ -> False
  ECast _ _ -> False

  ELoc e _ -> isBExp e 
  EParens _ -> False
  
  ELam _ -> False
  ELet _ -> False
  EIf _ _ _ -> False
  ECase _ _ -> False

  ERef _ -> False
  EDeref _ -> False
  
  ETuple _ _ -> False
  ECon _ [] -> False
  ECon _ _ -> True
  ENewCon _ _ -> True
  EFree _ -> True
  
  EGet _ _ -> False
  EGetI _ _ -> False
  ESet _ _ -> True

  ENewArray _ -> True
  ENewArrayI _ -> True
  EResizeArray _ _ -> True

  ENewVect _ -> True
  ENewVectI _ -> True

  ENewString _ -> True

  EOp _ -> True

isAExp :: Exp -> Bool
isAExp = \case
  EVar _ -> True
  ELit _ -> True
  EApp _ _ -> False

  EType _ _ -> False
  ECast _ _ -> False

  ELoc e _ -> isAExp e
  EParens _ -> True

  ELam _ -> False
  ELet _ -> False
  EIf _ _ _ -> False
  ECase _ _ -> False

  ERef _ -> True
  EDeref _ -> True
  
  ETuple _ _ -> True
  ECon _ [] -> True
  ECon _ _ -> False
  ENewCon _ _ -> False
  EFree _ -> False
  
  EGet e _ -> isAExp e
  EGetI e _ -> isAExp e
  ESet _ _ -> False

  ENewArray _ -> False
  ENewArrayI _ -> False
  EResizeArray _ _ -> False

  ENewVect _ -> False
  ENewVectI _ -> False

  ENewString _ -> False
  
  EOp _ -> False

isAType :: Type -> Bool
isAType = \case
  TArr _ _ -> False
  TCon _ -> True
  TInt _ -> True
  TUInt _ -> True
  TFp _ -> True
  TTuple _ _ -> True
  TArray _ _ -> True
  TVect _ _ -> True
  TPtr _ -> True
  TLoc ty _ -> isAType ty
  TParens _ -> True

isAPat :: Pat -> Bool
isAPat = \case
  PVar _ -> True
  PCon _ [] -> True
  PCon _ _ -> False
  PTuple _ _ -> True
  PWild -> True
  PType _ _ -> False
  PLoc p _ -> isAPat p
  PParens _ -> True

isBPat :: Pat -> Bool
isBPat = \case
  PVar _ -> False
  PCon _ [] -> False
  PCon _ _ -> True
  PTuple _ _ -> False
  PWild -> False
  PType _ _ -> False
  PLoc p _ -> isBPat p
  PParens _ -> False