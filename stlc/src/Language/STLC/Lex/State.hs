{-# LANGUAGE TemplateHaskell #-}
module Language.STLC.Lex.State where

import Lens.Micro.Platform
import Language.Syntax.Location
import Language.STLC.Lex.Token (Token)

data LexState =
  LexState  { _lexTokAcc :: [Token]
            , _lexRegion :: Region
            , _lexStartcode :: Int
            , _lexCommentDepth :: Int
            , _lexStringBuf :: String
            , _lexFilePath :: FilePath
            } deriving Show



initialLexState :: FilePath -> LexState
initialLexState fp =
  LexState
    { _lexTokAcc = []
    , _lexRegion = R (P 0 0) (P 0 0)
    , _lexStartcode = 0
    , _lexCommentDepth = 0
    , _lexStringBuf = ""
    , _lexFilePath = fp
    }

makeLenses ''LexState

instance HasLocation LexState where
  locOf s = Loc { _locPath = _lexFilePath s, _locReg = _lexRegion s }