{-# LANGUAGE  FlexibleInstances
            , BangPatterns
            , DeriveGeneric
            , OverloadedStrings
            , TemplateHaskell
            , DeriveDataTypeable
            , RecordWildCards
            , ViewPatterns
            , UndecidableInstances
  #-}
module Language.Syntax.Location where

import Lens.Micro.Platform

import Data.Data
import GHC.Generics (Generic)

import Data.Text.Prettyprint.Doc


data L a = L { unL :: a, unLoc :: Loc }



data Loc
  = Loc
    { _locPath  :: FilePath
    , _locReg   :: Region 
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)


data Region
  = R
    { _regStart :: Position
    , _regEnd   :: Position
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)


data Position
  = P
    { _posLine    :: Int
    , _posColumn  :: Int
    }
    deriving (Eq, Read, Show, Data, Typeable, Generic)


instance Ord Position where
    compare (P l1 c1) (P l2 c2)
      | l1 == l2  = c1 `compare` c2
      | otherwise = l1 `compare` l2

makeLenses ''Loc
makeLenses ''Region
makeLenses ''Position


-- -----------------------------------------------------------------------------
-- Getting Classy  

class HasLocation a where
  locOf :: a -> Loc

class HasRegion a where
  regOf :: a -> Region

class HasPosition a where
  posOf :: a -> Position


-- -----------------------------------------------------------------------------
-- Instances  

instance HasLocation (L a) where
  locOf = unLoc

instance Functor L where
  fmap f (L a l) = L (f a) l


instance Semigroup a => Semigroup (L a) where
  (<>) (L a l1) (L b l2) = L (a<>b) (l1<>l2)

instance HasLocation Loc where
  locOf = id

instance HasRegion Region where
  regOf = id

instance HasPosition Position where
  posOf = id


instance HasLocation a => HasRegion a where
  regOf = regOf . locOf


instance Semigroup Loc where
  (<>) (Loc fp1 r1) (Loc fp2 r2)
    | fp1 == fp2 = Loc fp1 $ r1 <> r2
    | otherwise  = error $ "Cannot mappend two locations with different file names." 


instance Semigroup Region where
    (<>) (R s1 e1) (R s2 e2)
      = R (min s1 s2)
          (max e1 e2 )

instance Monoid Region where
    mempty
      = R (P 0 0) (P 0 0)


-- Location can be taken from any foldable functor with location
instance {-# OVERLAPPABLE #-} (Foldable f, Functor f, HasLocation a) => HasLocation (f a) where
    locOf = foldl1 (<>) . fmap locOf

-- -----------------------------------------------------------------------------
-- Helpers

mkRegion :: (HasPosition a, HasPosition b) => a -> b -> Region
mkRegion start end = R (posOf start) (posOf end)

stretch :: HasPosition a => a -> Int -> Region
stretch a n = mkRegion p1 p2
  where
    p1@(P l c) = posOf a
    p2 = P l (c + n)


(<++>) :: (HasLocation a, HasLocation b) => a -> b -> Loc
(<++>) a b = locOf a <> locOf b

-- -----------------------------------------------------------------------------
-- Pretty Instances   

instance Pretty Loc where
    pretty Loc{..} =
      pretty _locPath <> ":" <> pretty _locReg


instance Pretty Region where
  pretty (R s e)
    | s == e
      = pretty s
    | otherwise
      = pretty s <> "-" <> pretty e


instance Pretty Position where
  pretty (P l c) =
    pretty (l+1) <> ":" <> pretty (c+1)
