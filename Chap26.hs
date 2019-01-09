{-# LANGUAGE InstanceSigs #-}

module Chap26 where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (Reader(..), Reader, reader)
import Chap25 (IdentityT(..), Identity(..), Identity)

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT mma) >>= f =
    MaybeT $ do
      v <- mma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT $ f y

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT fab) <*> (EitherT mea) = EitherT $ ((<*>) <$> fab <*> mea)

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f =
    EitherT $ do
      v <- mea
      case v of
        Left x -> return $ Left x
        Right y -> runEitherT $ f y

swapEither :: Either a b -> Either b a
swapEither (Right x) = Left x
swapEither (Left x) = Right x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mab) = mab >>= (either f g)

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (ReaderT fmab) <*> (ReaderT rma) =
    ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a
        -> (a -> ReaderT r m b)
        -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s ->
                 let r = sma s
                 in fmap (\(a, s') -> (f a, s')) r

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT smab) <*> (StateT sma) =
    StateT $
      \s -> do
        (a, s')   <- sma s
        (fa, s'') <- smab s'
        return (fa a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (>>=) :: (StateT s m a) -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $
                \s -> do
                  (a, s') <- sma s
                  runStateT (f a) s'

instance MonadTrans IdentityT where
  lift = IdentityT

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
                        a <- ma
                        return (a, s)

instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO

rDec :: Num a => Reader a a
rDec = reader $ flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \x -> do
                            putStrLn ("Hi: " ++ show x) 
                            return (x + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \x -> do
                             putStrLn ("Hi: " ++ show x)
                             return (show x, x + 1)