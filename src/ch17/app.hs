-- CH 17 APPLICATIVE

-- recap

-- monoid -> gives us a means of mashing two values of the same type together
-- functor -> is a function application over/through some structure we don't care about
-- applicative -> monoidal functors
--              lifted over structure (like Functor)
--              but the function we're applying is _also_ embedded in some structure
--              because the fn and the value it's being applied to both have structure
--                 we need to mash them together

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
  
-- note that f (the structure) IS A FUNCTOR
-- every type that can have an APPLICATIVE must also have a FUNCTOR

-- pure lifts something into the functorial (applicable) structure
-- kind of like ... structural identity
-- <*> is the infix operator called APPLY

-- fmap
-- (<$> :: Functor f)     => (a -> b) -> f a -> f b

-- applicative

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--                        ^^^^^^^^
--
-- the difference is the f is on the _outside_ of our function in applicative

-- you can define a FUNCTOR in terms of a APPLICATIVE
-- fmap f x = pure f <*> x 

-- fmap (+1) [1, 2, 3]
-- pure (+1) <*> [1..3]

-- pure has type Applicative f => a -> f a
-- think of it as embedding a value of any type into the structure we want

-- pure 1 :: [Int] -> [1]
-- pure 1 :: Maybe Int -> Just 1
-- pure 1 :: Either a Int -> Right 1
-- pure 1 :: ([a], Int) -> ([], 1)
-- remember the right cases from fmap .. it's the same here (e..g fmap (+1)(4, 5) -> (4,6)
--   and for either you can't provide a function that transforms EIther[String,Int] as you either have a function
--   on the Int or the String but not both at the same time.

-- 17.4 applicative functors are monoidal functors

-- ($)   ::   (a -> b) ->   a ->   b    regular right associative apply function (think ordinary function application)
-- (<$>) ::   (a -> b) -> f a -> f b    fmap
-- (<*>) $$ f (a -> b) -> f a -> f b    applicative 


-- Notice the two arguements to our function are f (a -> b) and f a 
-- f            f         f
-- (a -> b)     a         b
-- early we wrote something that can take values of one type and return one value of that type MONOID
-- mappend :: Monoid a => a -> a -> a
-- mappend ::      f          f      f
-- $       ::   (a -> b)      a      b
-- <*>     :: f (a -> b) -> f a -> f b

-- we are kind of bolting monoid onto functor to be able to deal with functions embedded in additional structure

-- list
-- e.g apply this list of functions to this list of ints 
--[(*2), (*3)] <*> [4,5]
-- = [2 *4, 2 *5, 3 *4, 3 * 5]

