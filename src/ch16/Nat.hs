{-# LANGUAGE RankNTypes #-}

module Nat where

type Nat f g = forall a . f a -> g a

-- doing the opposite of wht functor does

maybeToList :: Nat Maybe[]
maybeToList Nothing = []
maybeToList (Just a) = [a]
