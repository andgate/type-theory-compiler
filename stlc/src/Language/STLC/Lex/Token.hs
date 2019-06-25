{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           , DeriveGeneric
           , TypeFamilies
           , FlexibleInstances
           , BangPatterns
           , DeriveDataTypeable
  #-}
module Language.STLC.Lex.Token where

import Lens.Micro.Platform
import Data.Binary hiding (encode)
import Data.Data
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Language.Syntax.Location
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

instance HasLoc Token where
    loc = tokLoc

instance HasRegion Token where
    region = tokLoc . region

-- -----------------------------------------------------------------------------
-- Extraction

extractId :: Token -> L Text
extractId (Token c _ l) = case c of
  TokenVarId  n -> L l n
  TokenConId  n -> L l n
  _ -> error "unexpected token"


extractInteger :: Token -> L Integer
extractInteger (Token (TokenInteger v) _ l) = L l v
extractInteger _ = error "unexpected token"

extractDouble :: Token -> L Double
extractDouble (Token (TokenDouble v) _ l) = L l v
extractDouble _ = error "unexpected token"

extractChar :: Token -> L Char
extractChar (Token (TokenChar v) _ l) = L l v
extractChar _ = error "unexpected token"

extractString :: Token -> L String
extractString (Token (TokenString v) _ l) = L l v
extractString _ = error "unexpected token"

extractBool :: Token -> L Bool
extractBool (Token (TokenBool v) _ l) = L l v
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


-- -----------------------------------------------------------------------------
-- Serialization Instances

instance Binary Token
instance Binary TokenClass
