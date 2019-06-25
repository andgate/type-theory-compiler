{-# LANGUAGE LambdaCase #-}
module Language.Hawk.Parse.Error where

import Data.Text.Prettyprint.Doc
import Language.Hawk.Lex.Error
import Language.Hawk.Lex.Token
import Language.Hawk.Syntax.Concrete (Stmt)


data ParseError
    = UnexpectedToken [Token] [String]
    | AmbiguousGrammar [Stmt]
    | PLexErr LexError
    deriving(Show)

instance Pretty ParseError where
    pretty = \case
        UnexpectedToken unexpected expected ->
            vsep [ pretty "Unexpected tokens:" <+> dquotes (pretty unexpected)
                 , pretty "Expected tokens:" <+> dquotes (pretty expected)
                 ]
    
        AmbiguousGrammar srcs ->
            vcat [ pretty "Severe Parser Error: Ambiguous grammar encountered. Please report."
                 , vcat (pretty <$> srcs)
                 ]

        PLexErr err ->
            pretty err
