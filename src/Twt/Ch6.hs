{-# LANGUAGE RankNTypes #-}
module Twt.Ch6 where

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

cont :: a -> (forall r. (a->r)->r)
cont a = \callback -> callback a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f = let callback = id in f callback

instance Functor Cont where
  fmap f c =
    let cont = unCont c
        a = cont id
        b = f a
    in  Cont $ \callback -> callback b

instance Applicative Cont where
  -- | pure :: a -> f a
  pure a = Cont $ \callback -> callback a
  -- | (<*>) :: f (a -> b) -> f a -> f b
  cfunc <*> ca =
    let
      a = unCont ca $ id
      func = unCont cfunc $ id
      b = func a
    in
      Cont $ \callback -> callback b

instance Monad Cont where
  -- | (>>=)       :: forall a b. m a -> (a -> m b) -> m b
  ca >>= func =
    let
      a = unCont ca $ id
      cb = func a
      b = unCont cb $ id
    in
      Cont $ \callback -> callback b


withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

withOS :: (String -> r) -> r
withOS f = f "linux"

releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
  version <- Cont withVersionNumber
  date <- Cont withTimestamp
  os <- Cont withOS
  pure $ os ++ "-" ++ show version ++ "-" ++ show date
