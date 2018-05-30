module Exer where

import Control.Applicative


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil x = x
  mappend x Nil = x
  mappend (Cons x xs) ys = Cons x (xs `mappend` ys)  

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Cons f fs <*> y = (f <$> y) `mappend` (fs <*> y)

-- oh yeah that makes sense. Take the first function in the list and map it to y
-- then append is to the tail of the list of functions <*> to the whole list again
-- so its entire list of f1 results appended to entire list of f2 results appended to
-- entire list of f3 results
