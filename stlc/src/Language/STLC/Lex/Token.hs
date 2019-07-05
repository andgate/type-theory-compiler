{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           , DeriveGeneric
           , TypeFamilies
           , FlexibleInstances
           , BangPatterns
           , DeriveDataTypeable
  #-}
module Language.STLC.Lex.Token where

import Language.Syntax.Location

import Lens.Micro.Platform
import Data.Data
import GHC.Generics (Generic)

import Data.Text (Text, pack)
import Data.Text.Prettyprint.Doc

-- -----------------------------------------------------------------------------
-- Token Types

-- | A `Token` augmented with `Region` information
data Token = Token
    { _tokClass     :: !TokenClass
    , _tokText      :: !Text
    , _tokLoc       :: !Loc
    } deriving (Eq, Show, Ord, Data, Typeable, Generic)

-- The token type:
data TokenClass
  = TokenRsvp Text
  | TokenVarId Text
  | TokenConId Text
  | TokenPrimId Text

  | TokenInteger Integer
  | TokenDouble Double
  | TokenChar Char
  | TokenString String
  | TokenBool Bool

  | TokenLn
  | TokenLn'
  | TokenBlk
  | TokenBlk'
  | TokenEof
  deriving (Eq, Show, Ord, Data, Typeable, Generic)


makeLenses ''Token

instance HasLocation Token where
    locOf = _tokLoc


-- -----------------------------------------------------------------------------
-- Extraction

extractId :: Token -> L Text
extractId (Token c _ l) = case c of
  TokenVarId  n -> L n l
  TokenConId  n -> L n l
  _ -> error "unexpected token"


extractInteger :: Token -> L Integer
extractInteger (Token (TokenInteger v) _ l) = L v l
extractInteger _ = error "unexpected token"

extractDouble :: Token -> L Double
extractDouble (Token (TokenDouble v) _ l) = L v l
extractDouble _ = error "unexpected token"

extractChar :: Token -> L Char
extractChar (Token (TokenChar v) _ l) = L v l
extractChar _ = error "unexpected token"

extractString :: Token -> L String
extractString (Token (TokenString v) _ l) = L v l
extractString _ = error "unexpected token"

extractBool :: Token -> L Bool
extractBool (Token (TokenBool v) _ l) = L v l
extractBool _ = error "unexpected token"


-- -----------------------------------------------------------------------------
-- Pretty Instances

instance Pretty Token where
    pretty t =
      dquotes (pretty (t^.tokText))
        <+> parens (pretty (t^.tokClass)) 
        <+> "@"
        <> pretty (t^.tokLoc)

instance Pretty TokenClass where
    pretty tc =
      pretty (pack . show $ tc)