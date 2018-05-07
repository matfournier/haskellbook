module Unfold where

-- folds let us break structures down
-- unfolds let us build them back up

-- iterate is like a limited unfold that never ends

-- :t iterate
-- iterate :: (a -> a) -> a -> [a]
-- because it never ends, we must use take
-- to get a finte list
-- take 10 $ iterate(+1) 0
-- [0, 1, 2,3 ,4 ,5, 6, ...]

-- unfoldr is more general
-- :t unfoldr
-- unfoldr :: (b -> Maybe(a, b)) -> b -> [a]
-- using unfoldr to do the same thing as iterate
-- take $ 10 unfoldr (\b -> JUst (b, b+1)) 0

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f a =
  case f x of
    Nothing -> []
    Just (a, b) -> a : myUnfoldr f b 

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x 
