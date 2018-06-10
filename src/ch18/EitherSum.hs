module EItherSum where

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First x) = First x
  fmap f (Second x) = Second (f x)
  

instance Applicative (Sum a) where
  pure = Second
  (First f) <*> _ = First f
  _ <*> (First f) = First f
  (Second f) <*> (Second x) = Second (f x)
  

instance Monad (Sum a) where
  return = pure
  (Second x) >>= f = f x
  (First x) >>= _ = First x 

-- let x = Second 17
-- x >>= (\t -> Second(t+1)) will return Second 18

-- let z = First "Nope"
-- z >>= (\t -> Second(t+1)) will return First Nope
