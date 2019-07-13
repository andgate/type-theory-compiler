{-# LANGUAGE LambdaCase, OverloadedStrings, ViewPatterns #-}
module Language.STLC.Lex.Error where

import Language.Syntax.Location
import Data.Text.Prettyprint.Doc

data LexError
  = UnrecognizedToken Loc String
  | InvalidCharLit Loc String
  | InvalidEscapeChar Loc Char
  | IllegalLexerSkip Loc
  deriving(Show)


instance Pretty LexError where
    pretty = \case
      UnrecognizedToken (pretty -> l) (pretty -> cs) ->
        vsep [ line <> l <+> "error:"
             , indent 4 $ "Lexer has encountered an unrecognized token: " <+> dquotes cs
             ]

      InvalidCharLit (pretty -> l) (pretty -> str) ->
        vsep [ line <> l <+> "error:"
             , indent 4 $ "Lexer has encountered an invalid literal character:" <+> squotes str
             ]

      InvalidEscapeChar (pretty -> l) (pretty -> str) ->
        vsep [ line <> l <+> "error:" 
             , indent 4 $ "Lexer has encountered an invalid literal character:" <+> squotes str
             ]

      IllegalLexerSkip (pretty -> l) ->
        vsep [ line <> l <+> "error:" 
             , indent 4 $ "Lexer performed an illegal skip operation."
             ]
