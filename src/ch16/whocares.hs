module Whocares where

data WhoCares a =
  ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

-- this has one data constructor containing a value we could fmap over and that is Matter
-- others are nullary so there is no value to work inside that structure, there is only structure

-- law abiding instance

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

-- but you could make one that fucks with id

-- instance Functor WhoCares where
-- fmap _ ItDoesnt = WhatThisIsCalled
-- fmap f WhatThisIsCalled = ItDoesnt
-- fmap f (Matter a) = Matter (f a)
-- fmap id ItDoesnt would return WhatThisIsCalled

-- if you want a function that can change the value AND the structure, just use a regular old function
-- if you want to change the value but leave the structure untouched, us fmap

