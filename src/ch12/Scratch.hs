module Scratch where

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just(n+1) else Nothing

-- smart constructors

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

-- few problems, one is that we can construct a person w/ empty string or
-- make a person who is negative years old

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 =
    Just $ Person name age
  | otherwise = Nothing
  

-- data Either a b = Left a | Right b

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

mkPersonEither :: Name -> Age -> Either PersonInvalid Person
mkPersonEither name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow
