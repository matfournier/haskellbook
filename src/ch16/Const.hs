module Const where

import Data.Functor

-- notice that b is the phantom type

newtype Constant a b =
  Constant { getConstant :: a}
  deriving (Eq, Show)


instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v
  

data Wrap f a = Wrap (f a) deriving (Eq, Show)

-- notice our a here is an arguement to f?

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)
  
