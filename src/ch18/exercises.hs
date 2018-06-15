module Exercises where

data PbEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (PbEither b) where
  fmap f (PLeft x) = PLeft (f x)
  fmap f (PRight b) = PRight b

instance Applicative (PbEither b) where
  pure = PLeft
  (PRight f) <*> _ = PRight f
  _ <*> (PRight f) = PRight f
  (PLeft f) <*> (PLeft x) = PLeft (f x)

instance Monad (PbEither b) where
  return = pure
  (PRight x) >>= _ = PRight x
  (PLeft x) >>= f = f x 


-- identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor (Identity) where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

-- let x = Identity 3
-- x >>= (\x -> Identity (x+5))

instance Monad (Identity) where
  return = pure
  (Identity a) >>= f = f a 

-- list

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons head rest ) = Cons (f head) (fmap f rest)
  
instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys         = ys
  mappend (Cons x xs) ys = Cons x (xs `mappend` ys)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons firstFunc restFuncs <*> vs = mappend (firstFunc <$> vs) (restFuncs <*> vs)

instance Monad List where
  return a = Cons a Nil
  (Cons head rest) >>= f = f head `mappend` (rest >>= f)
  
-----

-- j :: Monad m => m (m a) -> m a
-- j = join

-- l1 :: Monad m => (a -> b) -> m a -> m b
-- l1 = liftM

-- l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- l2 :: liftM2

-- a :: Monad m => m a => m (a -> b) -> m b
-- not sure, something with flip?

-- meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- not sure, mapM something?
