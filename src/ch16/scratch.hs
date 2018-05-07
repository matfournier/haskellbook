module Scratch where 

-- a functor is a way to apply a function over or around some structure we don't want to alter
--   e.g. just what is inside of it--
-- 1           2  3  4
-- class Functor f where
--  fmap :: (a -> b) -> f a -> f b
--   5        6          7      8
-- 1 class to being def of typeclass
-- functor is name of type class
-- typeclass in haskell refer to a type, we've just called it f
--  where ends the declartion of typeclas name and associated types
--  operations provided on the typeclass come after
-- 5 we have an op named fmap
-- 6 which is a function of a -> b
-- 7 arguement f a is a functor f that takes an arguement a
--  that is f is a type that has an instance of the functor typelcass
-- 8 this f is the same f as in 7

-- map (\x -> x > 3) [1..6]  will be [f, f, f, t, t, t]
-- fmap(\x -> x > 3) [1..6] will be [f, f, f, t, t, t]

-- fmap (+1) (Just 1) -> Just 2
-- fmap (10/)(4, 5) will be 4,2.0

-- let rca = right "chris allen"
-- fmap (++ ", esq.") rca will be Right "Chris ALlen, esq"

-- SO LOOKING AT F
-- it must have the kind * -> * ... a type with kind * -> * is awaiting application to a type constant of
-- of kind *
-- 1 each arguement (and result) in type sig for a function must be fully a fully applied type.  Each
     -- arguement must have the kind *
-- the type f was applied to a single arguement in two different places: f a and f b, since f a and f b
   -- must each have the kind *, f by itself must be * -> *

-- :k (->)
--  (->) :: * -> * -> *
-- each arguement and result of every function must be a constant, not a type constructor

-- fmap :: (a -> b) -> f a -> f b
-- has kind   *     -> *   -> *
--
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- looks a lot like ($) :: (a -> b) -> a -> b  .. looks a lot like function application but THROUGH
--   some structure f

-- FUNCTOR LAWS

-- identity

-- fmap id == id
-- fmap id "Hi Julie" -> "Hi Hulie"
-- id "Hi Julie" -> "Hi Julie"
-- e.g. since the inner structure is just id, the OUTER structure should not change

-- composition

-- fmap (f . g) == fmap f . fmap g
-- fmap ((+1) . (*2))[1..5] == fmap(+1) . fmap(*2) $ [1..5] e.g. structure prserving 
--  see whocares.hs

-- composition should just work

-- commonly used functors

-- :t const
-- const -> a -> b -> a
-- let replaceWithP = const 'p'
-- replaceWithP 10000 'p'
-- replaceWithP "woohoo" p
-- replaceWithP (Just 10)
-- 'p'

-- data Maybe a = Nothing : Just a
-- fmap replaceWithP (Just 10)
-- just 'p'
-- fmap replacewithP Nothing
-- Nothing

-- tuples
-- data (,) a b = (,) a b
-- fmap replaceWithP (10,20) -> (10, 'p')
--    skips the first value, has to do with kindness?


-- functor for FUNCTIONS 
-- negate 10 -> -10
-- let tossEmOne = fmap(+1) negate
-- tossEmOne 10
-- -9
-- tossEmOne (-10)
-- 11 

-- functors are stacked.
--  lms is List(Maybe(String)) and string, maybe, and List all have their own functors
-- let n = Nothing
-- let w = Just 'woohoo'
-- let ave = Just "Ave"
-- if lms = [ave, n, w]
-- and let replaceWithP = const 'p'
-- replaceWithP lms will give p
-- (fmap . fmap) replaceWithP lms
-- [Just 'p', Nothing, Just 'p']
-- let tripFmap = fmap . fmap .fmap
-- tripFmap replaceWithP lms
-- [Just "ppp", Nothing, Just "pppppp"]

data Possibly a = LolNope | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)
  
incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e) = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

incEither :: Num a => Either e a -> Either e a
incEither m = fmap (+1) m

showEither ::: Show a => Either e a -> Either e String
showEither s = fmap show s

-- we can drop the obvious arugement and drop m in incEither if we wanted
-- but I don't like to?
-- e.g. IncEither = fmap (+1) 

-- this just generalizes to ->

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show


-- SOMETHING DIFFERENT

-- we alked about Functor as a means of lifting functions over structure so that we may
-- transform only conents
-- but what if we want to transform the STRUCTURE and leave the TYPE ARGUEMENT the same?
--   this is called a NATURAL TRASNFORMATION

-- e.g. nat :: (f -> g) -> f a -> g a
-- but this is impossible?
--  also: is this just not.... normal functions?
-- see Nat.hs


-- FUNCTORS ARE UNIQUE TO A DATATYPE
-- unlike monoid
