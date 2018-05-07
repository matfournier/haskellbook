module Pfunk where

replaceWithP :: a -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe[Char]] -> Char
replaceWithP' = replaceWithP

-- what happens if we lift it?
-- :t fmap replaceWithP
-- fmap replaceWithP :: Functor f => f a -> f Char

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- can we assert a more specific type?

liftedReplace' :: [Maybe[Char]] -> [Char]
liftedReplace' = liftedReplace
-- f is instantiated to [] (the f of f is the outermost [] in [Maybe[Char]]).

-- what if we lift twice?

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- making it more specific

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted
