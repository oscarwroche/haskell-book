{-# LANGUAGE InstanceSigs #-}

module Chap26 where

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
