{-# LANGUAGE LambdaCase #-}
module Language.STLC.Parse.Error where

import Language.STLC.Lex.Error
import Language.STLC.Lex.Token

import Data.Text.Prettyprint.Doc


data ParseError
    = UnexpectedToken [Token] [String]
    | PLexErr LexError
    deriving(Show)

instance Pretty ParseError where
    pretty = \case
        UnexpectedToken unexpected expected ->
            vsep [ pretty "Unexpected tokens:" <+> dquotes (pretty unexpected)
                 , pretty "Expected tokens:" <+> dquotes (pretty expected)
                 ]

        PLexErr err ->
            pretty err
