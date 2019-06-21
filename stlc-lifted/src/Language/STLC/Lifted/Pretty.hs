{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , FlexibleInstances
           #-}
module Language.STLC.Lifted.Pretty where

import Language.STLC.Lifted.Syntax

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name

import Data.Text.Prettyprint.Doc

import Data.Maybe

instance Pretty Module where
  pretty (Module n ds)
    = vsep (("module" <+> pretty n <> line):(pretty <$> ds))


instance Pretty Defn where
  pretty = \case
    FuncDefn f -> pretty (FreshP f) <> line
    ExternDefn ex -> pretty ex <> line
    DataTypeDefn dt -> pretty dt <> line


instance Pretty Extern where
  pretty (Extern n paramtys retty)
    = hsep ["extern", pretty n, ":", pretty (tarr paramtys retty)]


instance Pretty DataType where
  pretty (DataType n [])
    = "type" <+> pretty n

  pretty (DataType n (c:cs))
    = vsep [ "type" <+> pretty n
           , indent 2 (vsep (c':cs'))
           ]
    where
      (c_n, c_args) = c
      c_args'
        | length (catMaybes (fst <$> c_args)) == 0 = do
            (_, ty) <- c_args
            return $ pretty ty

        | otherwise = do
            (may_n, ty) <- c_args
            return $ (maybe "_" pretty may_n) <+> ":" <+> pretty ty

      c' = if length (catMaybes (fst <$> c_args)) == 0
             then "=" <+> pretty c_n <+> hsep c_args' 
             else "=" <+> pretty c_n <+> encloseSep lbrace rbrace comma c_args' 

      cs' = do
        (n, args) <- cs
        let args'
              | length (catMaybes (fst <$> args)) == 0 = do
                  (_, ty) <- args
                  return $ pretty ty
              | otherwise = do
                  (may_n, ty) <- args
                  return $ (maybe "_" pretty may_n) <+> ":" <+> pretty ty

        if length (catMaybes (fst <$> args)) == 0
          then return $ "|" <+> pretty n <+> hsep args'
          else return $ "|" <+> pretty n <+> encloseSep lbrace rbrace comma args'
        

instance Pretty Type where
  pretty = \case
    TArr a b -> pretty a <+> "->" <+> pretty b
    TCon n -> pretty n
    TI8 -> "I8"
    TI32 -> "I32"
    TArray i ty 
      | isAType ty -> pretty ty <> brackets (pretty i)
      | True       -> parens (pretty ty) <> brackets (pretty i)
    TPtr ty 
      | isAType ty -> pretty ty <> "*"
      | True       -> parens (pretty ty) <> "*"
    TString -> "String"
    TVoid -> "Void"


class PrettyFresh a where
  prettyFresh :: Fresh m => a -> m (Doc ann)

newtype FreshP a = FreshP a 

instance PrettyFresh a => Pretty (FreshP a) where
  pretty (FreshP a) = runFreshM $ prettyFresh a


instance PrettyFresh Func where
  prettyFresh (Func ty n bnd) = do
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

    EType e ty -> do
      e' <- prettyFresh e
      let ty' = pretty ty
      if isAExp e || isBExp e 
        then return $ hsep [e', ":", ty']
        else return $ hsep [parens e', ":", ty']
    
    EApp f xs -> do
      f' <- wrapBExpFresh f
      xs' <- mapM wrapBExpFresh xs
      return $ hsep (f':xs') 
    
    ELet bnd -> do
      (r_qs, e) <- unbind bnd
      let (ps, es) = unzip [ (p, e) | (p, Embed e) <- unrec r_qs]
      let ps' = pretty <$> ps
      es' <- mapM prettyFresh es
      let qs' = [ q' <+> "=" <+> e'
                | (q', e') <- zip ps' es' ]
      e' <- prettyFresh e
      return $ vsep [ "let" <+> align (vsep qs')
                    , "in" <+> e'
                    ]
      

    ECase e cls -> do
      e' <- prettyFresh e
      cls' <- mapM prettyFresh cls
      return $ vsep $ [ "case" <+> e' <+> "of"
                      , indent 2 (vsep cls')]
    

    EInt i -> return $ pretty i
    EString str -> return $ dquotes (pretty str)
    
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

    EDeref e -> do
      e' <- prettyFresh e
      if isAExp e
        then return $ "&" <> e'
        else return $ "&" <> parens e'

    ERef e -> do
      e' <- prettyFresh e
      if isAExp e
        then return $ "*" <> e'
        else return $ "*" <> parens e'

    EMember e m -> do
      e' <- prettyFresh e
      if isAExp e
        then
          return $ e' <> "." <> pretty m
        else
          return $ parens e' <> "." <> pretty m

    EOp op -> prettyFresh op

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
  
    PWild -> "_"
    PType p ty -> hsep [pretty p, ":", pretty ty]

instance PrettyFresh Clause where
  prettyFresh (Clause bnd) = do
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
    OpAddI a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["add", a', b']

    OpMulI a b -> do
      a' <- wrapBExpFresh a
      b' <- wrapBExpFresh b
      return $ hsep ["mul", a', b']
      

wrapBExpFresh :: Fresh m => Exp -> m (Doc ann)
wrapBExpFresh e 
  | isAExp e = prettyFresh e
  | True     = parens <$> prettyFresh e


isBExp :: Exp -> Bool
isBExp = \case
  EVar _ -> False
  EType _ _ -> False
  EApp _ _ -> True
  ELet _ -> False
  ECase _ _ -> False
  EInt _ -> False
  EString _ -> False
  ECon _ [] -> False
  ECon _ _ -> True
  ENewCon _ _ -> True
  EFree e -> True
  EDeref _ -> False
  ERef _ -> False
  EMember e _ -> False
  EOp _ -> True

isAExp :: Exp -> Bool
isAExp = \case
  EVar _ -> True
  EType _ _ -> False
  EApp _ _ -> False
  ELet _ -> False
  ECase _ _ -> False
  EInt _ -> True
  EString _ -> True
  ECon _ [] -> True
  ECon _ _ -> False
  ENewCon _ _ -> False
  EFree e -> False
  EDeref _ -> True
  ERef _ -> True
  EMember e _ -> isAExp e 
  EOp _ -> False

isAType :: Type -> Bool
isAType = \case
  TArr _ _ -> False
  TCon _ -> True
  TI8 -> True
  TI32 -> True
  TArray _ _ -> True
  TPtr _ -> True
  TString -> True
  TVoid -> True

isAPat :: Pat -> Bool
isAPat = \case
  PVar _ -> True
  PCon _ [] -> True
  PCon _ _ -> False
  PWild -> True
  PType _ _ -> False

isBPat :: Pat -> Bool
isBPat = \case
  PVar _ -> False
  PCon _ [] -> False
  PCon _ _ -> True
  PWild -> False
  PType _ _ -> False