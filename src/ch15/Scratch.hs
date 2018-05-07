-- a monoid is a binary associative operation with an idenity

-- mappend [1, 2, 3] [4, 5, 6]
-- mapend comes from the Monoid typeclass and it will
-- return [1,2,3,4,5,6]

--mappend [1..5][] = [1..5]

-- mappend x mempty = x
-- mappend mempty x = x

-- a monoid is a fn that takes 2 arguements and follows two laws
--  1) associativity - arguements can be regrouped in dif orders but will return same result
--  2) idenity - there exists some value that renders the operation moot and returns
--     e.g adding 0, multiplying by one

-- how Typeclass Monoid is defined

-- class Monoid m where
--   mempty :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m
--   mconcat = foldr mappend mempty

-- examples
-- mappend [1,2,3,] [4,5,6] will return 1,2,3,4,5,6
-- e.g. (++) [1, 2, 3] [4, 5, 6]
-- mconcat [[1..3], [4..6]] will return 1,2,3,4,5,6
-- e.g. foldr (++) [][[1..3], [4..6]] -> [1,2,3,4,5,6]
-- e.g. foldr mappend mempty [[1..3], [4..6]]

-- if we look up Monoid for list
-- instance Monoid [a] where
--   mempty = []
--   mappend = (++)

-- import Data.Monoid
-- :info Sum
-- newtype Sum a = Sum {getSum :: a}
-- instance Num a => Monoid (sum a)
-- :info Product
-- newtype Product a = Product {getProduct :: a}
-- instance Num a => Monoid (product a)
-- the instances say we can use SUm or Product values as a Monoid as long as they contain numeric values

-- :t (<>)
-- (<>) :: Monoid m => m -> m -> m
-- (Sum 8) <> (Sum 9)
-- Sum {getSum = 17}
-- mappend mempty Sum 9
-- Sum {getSum = 9}
-- but mappend (sum 8) (sum 9) (sum 10) won't work
-- mappend (Sum 1) (Mappend (Sum 2) (Sum 3)) will work
-- Sum 1 <> Sum 1 <> Sum 1
-- Sum {getSUm = 3}

-- mconcat [Sum 8, sum 9, sum 10] -> SUm {getSum = 27}
-- getSum $ mappend (Sum 1) (Sum 1) -> 2

-- big take away <> IS MAPPEND

-- many datatypes have more than one valid monoid
-- most convienent to use newtypes to tell them apart (e.g. the sum and product wrappers)
-- addition is the classic appending operation, as is concat
-- for other datatypes.. it's less clear
-- think of it more as a way to condense any set of values to a summary value

-- monoid for bool
-----------------------------

-- import Data.Monoid
-- All true <> All true
-- All {getAll = True}
-- All True <> All false
-- Al {getAll = false}
-- Any True <> Any False
-- ANy {getAny = True}

-- monoid for maybe
-------------------------------------
-- MORE THAN TWO POSSIBLE MONOIDS
-- two examples: First and LAst.
--     like boolean disjunction but explicit preference for the leftmost or rightmost success in a series
--     of Maybe values
-- First (Just 1) `mappend` First (Just 2)
-- -> First {getFirst = Just 1}

-- other Maybe Monoids

-- instance Monoid b => Monoid (a -> b)
-- instance (Monoid a, Monoid b) => Monoid (a,b)
-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)
-- these give us a monoid for a larger type by reusing the monoid instances of types that reprsent
--  components of the larger type
--  which is weird because Nothing doesn't have an a?

-- data Booly a = False' | True' deriving (Eq, Show)
-- instance Monoid (Booly a) where
--   mappend False' _ = False'
--   mappend _ False ' = False'
--   mappend True' True' = True'
--   we didn't need the monoid constraint for a because we're never mappending a values
--   fundamental reason why we don't need the constaint, but it can happen that we don't do this even
--   when the type does occur in the datatype

import Data.Monoid 

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada (Only b) = Only b
  mappend (Only b) Nada = Only b
  mappend (Only b) (Only c) = Only(b <> c)
  mappend Nada Nada = Nada
