{-# LANGUAGE FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , ViewPatterns
           , LambdaCase
           , CPP
           , UndecidableInstances
           #-}
module Control.Monad.Report where


import Control.Monad
import Control.Monad.Reader
-- import Control.Monad.Trans
import Data.Bifunctor
import Data.DList (DList)
import qualified Data.DList as DL

import Data.Functor.Identity
import Unbound.Generics.LocallyNameless


-- --------------------------------------------------------------------------
-- | Multi Error Datatype

data Result e a
  = Success a
  | Failure (DList e)
  | Failing (DList e) a


instance Functor (Result e) where
  fmap f = \case
    Success a -> Success (f a)
    Failure e -> Failure e
    Failing e a -> Failing e (f a)


instance Bifunctor Result where
  bimap f g = \case
    Success a    -> Success (g a)
    Failure es   -> Failure (f <$> es)
    Failing es a -> Failing (f <$> es) (g a)

  first f = \case
    Success a   -> Success a
    Failure es    -> Failure (f <$> es)
    Failing es a -> Failing (f <$> es) a

  second g = \case
    Success a -> Success (g a)
    Failure es -> Failure es
    Failing es a -> Failing es (g a)


-- --------------------------------------------------------------------------
-- | The `Report` monad.


type Report e a = ReportT e Identity a

runReport :: Report e a -> Either [e] a
runReport = runIdentity . runReportT


-- --------------------------------------------------------------------------
-- | The `ReportT` monad transformer.

newtype ReportT e m a
  = ReportT { unReportT :: Result e () -> m (Result e a) }

runReportT :: Monad m => ReportT e m a -> m (Either [e] a)
runReportT m = do
  r <- unReportT m (Success ())
  case r of
    Success   a -> return $ Right a
    Failure  es -> return $ Left $ DL.toList es
    Failing _ a -> return $ Right a


instance Functor m => Functor (ReportT e m) where
  fmap f (ReportT r) = ReportT $ \es -> fmap f <$> r es


instance (Functor m, Monad m) => Applicative (ReportT e m) where
  pure a = ReportT $ \case
    Success _    -> return $ Success a
    Failure es   -> return $ Failure es
    Failing es _ -> return $ Failing es a

  (ReportT mf) <*> (ReportT mx) = ReportT $ \es ->
    do  rf <- mf es
        case rf of
          Success f ->
            do  rx <- mx (Success ())
                case rx of
                  Success      x -> return $ Success      (f x)
                  Failure  es2   -> return $ Failure  es2
                  Failing  es2 x -> return $ Failing  es2 (f x)

          Failure  es'  -> return $ Failure es'
          
          Failing es' f ->
            do  rx <- mx $ Failing es' ()
                case rx of
                  Success       x -> return $ Success       (f x)
                  Failure  es''   -> return $ Failure  es''
                  Failing  es'' x -> return $ Failing  es'' (f x)
          


instance Monad m => Monad (ReportT e m) where
#if !(MIN_VERSION_base(4,8,9))
  return a = ReportT $ \case
    Success    _ -> return $ Success    a
    Failure es   -> return $ Failure es
    Failing  es _ -> return $ Failing  es a
#endif

  (ReportT ma) >>= k = ReportT $ \es -> do
    ra <- ma es 
    case ra of
      Success     a -> unReportT (k a) (Success ())
      Failure es'   -> return $ Failure es'
      Failing es' a -> unReportT (k a) (Failing es' ())


instance MonadTrans (ReportT e) where
  lift m = ReportT $ \case
      Success _ -> Success `liftM` m
      Failure es -> return $ Failure es
      Failing es _ -> Failing es `liftM` m


instance (MonadReader r m) => MonadReader r (ReportT e m) where
  ask = lift ask
  local f (ReportT m)
    = ReportT $ \r -> local f (m r)
  reader = lift . reader

instance Fresh m => Fresh (ReportT e m) where
  fresh = lift . fresh

-- --------------------------------------------------------------------------
-- | The `MonadReport` typeclass.

class MonadReport e m | m -> e where
  witness :: e -> m ()
  nonfatal :: e -> a -> m a
  fatal :: e -> m a
  allFatal :: m a -> m a
  report :: Result e a -> m a


instance Monad m => MonadReport e (ReportT e m) where
  witness = report . (`Failing` ()) . pure

  nonfatal e a = report $ Failing (pure e) a

  fatal = report . Failure . pure

  allFatal m = ReportT $ \case
      Failure es    -> return $ Failure es
      Failing es _ -> do
          ja <- unReportT m $ Success ()
          return $ case ja of
            Failure es'   -> let es'' = es' <> es in es'' `seq` Failure es''
            Failing es' _ -> let es'' = es' <> es in es'' `seq` Failure es''
            Success     a -> Failing es a
      Success _   -> do
          ja <- unReportT m $ Success ()
          return $ case ja of
              Failure es'   -> Failure es'
              Failing es' _ -> Failure es'
              Success     a -> Success a


  report ra = ReportT $ \r ->
    return $ case r of
      Success _ -> ra
      Failure es -> Failure es
      Failing es _ -> 
        case ra of
          Success a     -> Failing es a 
          Failure es'   -> let es'' = es' <> es in es'' `seq` Failure es''
          Failing es' a -> let es'' = es' <> es in es'' `seq` Failing es'' a
