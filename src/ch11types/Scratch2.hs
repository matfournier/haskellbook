{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scratch2 where

--newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

--instance TooMany Goats where
--  tooMany (Goats n) = tooMany n

-- Goats instance will do the same thing as the Int instance but we still
-- have to define it separately

-- but with the generalizednewtypederiving
newtype Goats = Goats Int deriving (Eq, Show, TooMany)
-- this picks up the underlying typelcass for int



