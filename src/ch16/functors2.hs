module Functors2 where

data FixMePls a = FixMe | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- fmap (+1)(Pls 1) will be Pls 2

-- data FixMePlsTwo a = FixMeToo | PlsToo a
  -- deriving(Eq, Show)

-- instance Functor (FixMePlsTwo a) where
--   fmap _ FixMeToo = FixMeToo
--   fmap f (PlsToo a) = Pls (f a)
-- won't compile, only has Kind *    
