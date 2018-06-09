module Scratch where

import Control.Monad(join)
import Control.Applicative((*>)) 
-- monads are like applicative functors

-- class Applicative m => Monad m where
-- (>>=) :: m a -> (a -> m b) - > m b
-- (>>) :: m a -> m b -> m b
-- return :: a -> m a    -- THIS IS THE SAME AS PURE 
-- monad stronger than applicative stronger than functor
-- you can derive applicative and functor in terms of monad
--   just as you can derive functor in terms of applicative

fmapViaMonad f xs = xs >>= return . f 

-- whenever you implement an instance of Monad for a type
--  you necessarily have an Applicative and a Functor as well

-- CORE
---------------
-- return :: a -> m a -- this is the same as pure
-- (>>) m a -> m b -> m b
--   sequences two actions while discarding any resulting value of the first action    
-- (>>=) :: m a -> (a -> m b) - > m b
-- BIND / FlatMap what have you
--  similar to fmap and <*>
-- e.g.

-- fmap :: Functor f
--      => (a -> b) -> f a -> f b
-- <*> :: Applicative f
--      => f (a -> b) -> f a -> f b
-- >>= :: Monad f
--      => f a -> (a -> f b) -> f b

-- quite similar but the first two arguements are kind of flipped

-- we can demonstrate this by mapping a function of a -> m b
-- fmap :: Functor f => (a -> f b) -> f a -> f ( f b)
  -- WE DOUBLE UP ON THE F (which is why flatten works)

addOne x = [x, 1]

-- :t fmap Addone [4,5 6]
-- fmap addOne [4, 5, 6] :: Num t => [[t]]
-- fmap AdOne [4,5, 6]
-- [[4, 1], [5,1], [6,1]]
-- our mapped structure has generated ONE MORE STRUCTURE
-- monad gives us a way to _discard this structure__

-- concat $ fmap AddOne [4, 5, 6]
-- concat :: Foldable t => t [a] -> [a]
-- concat :: [[a]] -> [a]
-- monad is a generalization of concat
-- unique part of monad is:

-- import Control.Monad (join)
-- join :: Monad m => m (m a) -> m a

-- so fmap + join = monad

-- BAD flipped version of fmap

myBind :: Monad m => (a -> m b) -> m a -> m b
myBind f ma = join $ fmap f ma 

-- yay addOne >>= [3,4,5] is the same as Mybind addOne [3,4,5]

-- monads also lift
-- there are a set of lift operations like we saw for Applicative
-- they don't do anything different but are a holdover from when before applicative was discovered
-- liftA :: Applicitive f
--       => (a -> b) -> f a -> f b
-- liftM :: Monad m
--       => (a1 -> r) -> m a1 -> m r '
-- remember that Lift is just fmap w/ a different typeclas constraint

-- there is also liftM2 much like liftA2 ...
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

-- liftM2 (,) (Just 3) (Just 5)
-- Just (3, 5)
-- same as (,) <$> Just 3 <*> Just 5


-- zipWith on lists is liftA2 / LiftM2 applied to list
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith (+) [3, 4] [5, 6]
-- [8, 10]
-- same as liftM2 (+) [3, 4] [5, 6] BUT WAIT a different monoid is used
-- so this ends up as 8, 9, 9, 10 :(

-- you also get 3s ..

-- DO SYNTAX ---------------------------------
--------------------------------------------------------------------

-- works with any monad
-- is just sugar

-- (*>) :: Applcative f => f a -> f b -> f b
-- (>>) :: Monad      m => m a -> m b -> m b
-- these are sequencing functions but really do the same thing

-- putStrLn "Hello, " *> putStrLn "World"  gives Hello, World! 

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>          -- >> and *> are the same remember 
  putStrLn "another thing" *>
  putStrLn "even another thing!"
  
binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn
  
-- where >>= comes into play here
-- putStrLn :: String -> IO ()
-- getLine :: IO String

-- if we just mapped putStrLn over getLine it wouldn't work
-- putStrLn <$> getLine
-- :t of this is actually IO (IO ()) 
-- remember that <$> :: Functor f => (a -> b) -> f a -> f b
-- our (a -> b) is putStrLn
--  String -> IO ()
-- so now our _b_ type is of type IO
-- so f (b) is going to jam an IO inside another IO

--   [1]  [2] [3]
h :: IO ( IO  ())
h = putStrLn <$> getLine

-- 1 the outermost IO structure represents the effect getLine must perform to get you a string
-- 2 this inner IO is the effect that would be preformed IF putStrLn was evaluated
-- 3 unit here is the unit putSTrLn returns

-- so we need to join those two things to eval them
-- just remembner haskell is lazy...
--     let printOne = putStrLn "1"
--     let printTwo = putStrLn "2"
--     let twoPrints = (printOne, printTwo)
--     :t twoActions ( IO(), IO())
--   fst twoActions will print 1, snd twoActions will print 2

-- so what we need to do is join those two ios together

-- join $ putStrLn <$> getLine works
-- :t of this is IO () 
-- this _merges_ the effects of getLine and putStrLn into a _single IO_ action

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls: "
  name <- getLine
  putStrLn ("hello there: " ++ name)

-- you can see why this is a pain in the ass
-- reemmber that >>= Monad m => m a -> (a -> m b) -> m b 
bindingAndSequencing' :: IO()
bindingAndSequencing' =
  putStrLn "name please" >>
  getLine >>=
  \name -> putStrLn ("hello there: " ++ name)

-- 759
