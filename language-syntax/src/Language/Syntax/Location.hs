{-# LANGUAGE  FlexibleInstances
            , BangPatterns
            , DeriveGeneric
            , OverloadedStrings
            , TemplateHaskell
            , DeriveDataTypeable
  #-}
module Language.Syntax.Location where

import Lens.Micro.Platform
import Data.Binary
import Data.Data
import Data.Foldable
import Data.Monoid
import Data.Text (pack)
import GHC.Generics (Generic)

import Data.Text.Prettyprint.Doc


-- Location wrapper
data L a = L { unLoc :: Loc
             , unL   :: a
             }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)


instance Functor L where
  fmap f (L l a) = L l (f a) 

wrapL :: (a, Loc) -> L a
wrapL (x, l) = L l x

unwrapL :: L a -> (a, Loc)
unwrapL (L l x) = (x, l)


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


makeClassy ''Loc
makeClassy ''Region
makeClassy ''Position

-- -----------------------------------------------------------------------------
-- Classy Instances  

class Locatable a where
    locOf :: a -> Loc

-- Location can be taken from any foldable functor
instance {-# OVERLAPPABLE #-} (Foldable f, Functor f, Locatable a) => Locatable (f a) where
    locOf = fold . fmap locOf

instance HasRegion Loc where
    region = locReg . region


-- Can't make a HasPosition instance for region, since it has two positions!

-- -----------------------------------------------------------------------------
-- Helpers

mkRegion :: HasPosition a => a -> a -> Region
mkRegion start end = R (start^.position) (end^.position)

stretch :: HasPosition a => a -> Int -> Region
stretch a n = mkRegion p1 p2
  where
    p1@(P l c) = a^.position
    p2 = P l (c + n)

-- -----------------------------------------------------------------------------
-- Helper Instances

instance Semigroup Loc where
    (<>) (Loc fp r1) (Loc _ r2)
      = Loc fp (r1 <> r2)

instance Monoid Loc where
    mempty = Loc "" mempty

instance Semigroup Region where
    (<>) (R s1 e1) (R s2 e2)
      = R (min s1 s2)
          (max e1 e2 )

instance Monoid Region where
    mempty
      = R (P 0 0) (P 0 0)


-- -----------------------------------------------------------------------------
-- Pretty Instances   

instance Pretty a => Pretty (L a) where
    pretty (L loc a) =
      vsep
        [ pretty a
        , "located at" <+> pretty loc
        ]


instance Pretty Loc where
    pretty loc =
      (pretty . pack $ loc^.locPath) <> ":" <> pretty (loc^.locReg)


instance Pretty Region where
  pretty (R s e)
    | s == e
      = pretty s
    | otherwise
      = pretty s <> "-" <> pretty e


instance Pretty Position where
  pretty (P l c) =
    pretty (l+1) <> ":" <> pretty (c+1)


-- -----------------------------------------------------------------------------
-- Binary Instances

instance Binary a => Binary (L a)
instance Binary Loc
instance Binary Region
instance Binary Position
