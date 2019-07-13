{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase
           , OverloadedStrings
           , ViewPatterns
           #-}
module Language.LLTT.Pretty where

import Language.LLTT.Syntax
import Data.Text.Prettyprint.Doc
import qualified Data.List.NonEmpty as NE


-----------------------------------------------------------------------
-- Module and Definitions
-----------------------------------------------------------------------

instance Pretty Module where
  pretty (Module _ n ds)
    = vsep (("module" <+> pretty n <> line):(pretty <$> ds))

instance Pretty Defn where
  pretty = \case
    FuncDefn f -> pretty f <> line
    ExternDefn ex -> pretty ex <> line
    DataTypeDefn dt -> pretty dt <> line


instance Pretty Func where
  pretty (Func _ n ps body) =
    let ps' = do
          p <- ps
          let p' = pretty p
          return $ if isAPat p then p' else parens p'
        body' = pretty (exEAnn body)
        retty = exType body
        paramtys = [pty | PType _ pty <- ps]
        ty = if null paramtys
              then retty
              else TFunc retty (NE.fromList [argty | PType _ argty <- ps])

    in case ps' of
      [] -> if isAExp body || isBExp body
              then vsep [ pretty n <+> ":" <+> pretty ty
                        , pretty n <+> "=" <+> body'
                        ]
              else vsep [ pretty n <+> ":" <+> pretty ty
                        , pretty n <+> "="
                        , indent 2 body'
                        ]

      _ -> if isAExp body || isBExp body
              then vsep [ pretty n <+> ":" <+> pretty ty
                        , pretty n <+> hsep ps' <+> "=" <+> body'
                        ]
              else vsep [ pretty n <+> ":" <+> pretty ty
                        , pretty n <+> hsep ps'
                        , indent 2 ("=" <+> align body')
                        ]


instance Pretty Extern where
  pretty (Extern _ n paramtys retty)
    = hsep ["extern", pretty n, ":", pretty (TFunc retty (NE.fromList paramtys))]


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


-----------------------------------------------------------------------
-- Expressions
-----------------------------------------------------------------------

instance Pretty Exp where
  pretty = \case
    EVar n -> pretty n
    
    ELit l -> pretty l

    ECall f xs ->
      let f' = wrapBExp f
          xs' = wrapBExp <$> (NE.toList xs)
      in hsep (f':xs') 

    EType e ty ->
      let e' = pretty e
          ty' = pretty ty
      in if isAExp e || isBExp e 
        then hsep [e', ":", ty']
        else hsep [parens e', ":", ty']
    
    ECast e ty ->
      let e' = pretty e
          ty' = pretty ty
      in if isAExp e || isBExp e 
        then hsep [e', "as", ty']
        else hsep [parens e', "as", ty']

    ELoc e _ -> pretty e
    EParens e -> parens $ pretty e

    ELet qs body ->
      let (ps, es) = unzip (NE.toList qs)
          ps' = pretty <$> ps
          es' = pretty <$> es
          qs' = zipWith (\q' e' -> q' <+> "=" <+> e') ps' es'
          body' = pretty body
      in vsep [ "let" <+> align (vsep qs')
              , "in" <+> body'
              ]

    EIf p t f -> 
      vsep ["if" <+> pretty p
           , indent 2 ("then" <+> pretty t)
           , indent 2 (pretty f)
           ]      

    EMatch e cls ->
      let e' = pretty e
          cls' = pretty <$> cls
      in vsep $ [ "match" <+> e' <+> "of"
                , indent 2 (vsep $ NE.toList cls') ]


    ERef e ->
      let e' = pretty e
      in if isAExp e
          then "&" <> e'
          else "&" <> parens e'

    EDeref e ->
      let e' = pretty e
      in if isAExp e
          then "*" <> e'
          else "*" <> parens e'

    ETuple x (NE.toList -> xs) ->
      tupled $ pretty <$> (x:xs)

    ECon n [] -> pretty n

    ECon n args ->
      pretty n <+> hsep (wrapBExp <$> args)


    ENewTuple x (NE.toList -> xs)
      -> "new" <+> tupled (pretty <$> (x:xs))

    ENewCon n [] ->
      "new" <+> pretty n

    ENewCon n args -> 
      "new" <+> pretty n <+> hsep (wrapBExp <$> args)

    EFree e -> "free" <+> wrapBExp e


    EGet e m ->
      wrapBExp e <> "." <> pretty m

    EGetI e i ->
      wrapBExp e <> brackets (pretty i)

    ESet lhs rhs -> 
      wrapBExp lhs <+> "<-" <+> wrapBExp rhs


    ENewArray xs -> "new" <+> list (pretty <$> xs)
    ENewArrayI i -> "new" <+> "Array" <> brackets (pretty i)
    EResizeArray e i -> "resize" <+> wrapBExp e <> brackets (pretty i)

    ENewVect xs -> "new" <+> encloseSep "<[" "]>" "," (pretty <$> xs)
    ENewVectI i -> "new" <+> "Vect" <> enclose "<[" "]>" (pretty i)

    ENewString str -> "new" <+> dquotes (pretty str)

    EOp op -> pretty op


instance Pretty Lit where
  pretty = \case
    LNull -> "null"
    LInt i -> pretty i
    LDouble d -> pretty d
    LBool b -> pretty b
    LChar c -> squotes $ pretty c
    
    LString str -> dquotes $ pretty str
    
    LArray xs -> list $ pretty <$> xs
    LArrayI i -> "Array" <> brackets (pretty i)
    
    LVect xs -> encloseSep "<[" "]>" "," (pretty <$> xs)
    LVectI i -> "Vect" <> enclose "<[" "]>" (pretty i)
    
    LGetI e i -> pretty e <> brackets (pretty i)


instance Pretty Else where
  pretty = \case
    Else _ e -> "else" <+> pretty e
    Elif _ p t f -> vsep [ "elif" <+> pretty p
                       , "then" <+> pretty t
                       , pretty f
                       ]


instance Pretty Op where
  pretty = \case
    OpAdd a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#add", a', b']

    OpSub a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#sub", a', b']

    OpMul a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#mul", a', b']

    OpDiv a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#div", a', b']

    OpRem a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#rem", a', b']

    OpNeg a ->
      let a' = wrapBExp a
      in hsep ["#neg", a']
    
    OpAnd a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#and", a', b']

    OpOr a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#or", a', b']

    OpXor a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#xor", a', b']

    OpShR a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#shr", a', b']

    OpShL a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#shl", a', b']

    OpEq a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#eq", a', b']

    OpNeq a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#neq", a', b']


    OpLT a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#lt", a', b']

    OpLE a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#le", a', b']

    OpGT a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#gt", a', b']

    OpGE a b ->
      let a' = wrapBExp a
          b' = wrapBExp b
      in hsep ["#ge", a', b']



-----------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------

instance Pretty Type where
  pretty = \case
    TVar n -> pretty n
    TCon n -> pretty n
    TInt s -> "I" <> pretty s
    TUInt s -> "U" <> pretty s
    TFp s -> "F" <> pretty s
    TTuple t (NE.toList -> ts) ->
      tupled $ pretty <$> (t:ts) 
    TArray i ty 
      | isAType ty -> pretty ty <> brackets (pretty i)
      | True       -> parens (pretty ty) <> brackets (pretty i)
    TVect i ty 
      | isAType ty -> pretty ty <> brackets (pretty i)
      | True       -> parens (pretty ty) <> enclose "<[" "]>" (pretty i)
    TPtr ty 
      | isAType ty -> "*" <> pretty ty
      | True       -> "*" <> parens (pretty ty)
    TFunc retty paramtys ->
      concatWith (\x y -> x <+> "->" <+> y)
                 (pretty <$> (NE.toList paramtys ++ [retty]))
    TLoc t _ -> pretty t
    TParens t -> parens $ pretty t
 
-----------------------------------------------------------------------
-- Patterns
-----------------------------------------------------------------------     

instance Pretty Pat where
  pretty = \case
    PVar v -> pretty v
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


instance Pretty Clause where
  pretty (Clause n [] e)
    | isAExp e || isBExp e
        = hsep [pretty n, "->", pretty e]
    
    | otherwise
        = vcat [ pretty n <+> "->"
               , indent 2 (pretty e)
               , mempty
               ]

  pretty (Clause n xs e) =
    let n'  = pretty n
        xs' = maybe "_" pretty <$> xs
    in if isAExp e || isBExp e then
         hsep [n', hsep xs', "->", pretty e]
       else
         vcat [ hsep [n', hsep xs', "->"]
              , indent 2 (pretty e)
              , mempty
              ]

-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------

wrapBExp :: Exp -> Doc ann
wrapBExp e 
  | isAExp e = pretty e
  | True     = parens $ pretty e


isBExp :: Exp -> Bool
isBExp = \case
  EVar _ -> False
  ELit _ -> False
  ECall _ _ -> True
  
  EType _ _ -> False
  ECast _ _ -> False

  ELoc e _ -> isBExp e
  EParens _ -> False

  ELet _ _ -> False
  EIf _ _ _ -> False
  EMatch _ _ -> False

  ERef _ -> False
  EDeref _ -> False
  
  ETuple _ _ -> False
  ECon _ [] -> False
  ECon _ _ -> True
  ENewTuple _ _ -> True 
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
  ECall _ _ -> False

  EType _ _ -> False
  ECast _ _ -> False

  ELoc e _ -> isAExp e
  EParens _ -> True
  
  ELet _ _ -> False
  EIf _ _ _ -> False
  EMatch _ _ -> False

  ERef _ -> True
  EDeref _ -> True
  
  ETuple _ _ -> True
  ECon _ [] -> True
  ECon _ _ -> False
  ENewTuple _ _ -> False
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
  TVar _ -> True
  TCon _ -> True
  TInt  _ -> True
  TUInt _ -> True
  TFp   _ -> True
  TTuple _ _ -> True
  TArray _ _ -> True
  TVect _ _ -> True
  TPtr _ -> True
  TFunc _ _ -> False
  TLoc t _ -> isAType t
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