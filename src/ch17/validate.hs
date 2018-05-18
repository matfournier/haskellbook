module Validate where


import Control.Applicative

validateLength :: Int
  -> String
  -> Maybe String

validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)


-- doens't work since we are expecing Address -> b
-- but we have Maybe (Address -> Person)
-- this is really
-- fmap (fmap Person (mkName "Babe")) (mkAddress "old macdonalds")
-- our a -> b is inside maybe
-- remember fmap :: Functor f => (a -> b) -> f a -> f b
-- remember :t fmap Person (mkName "Babe") leaves a _partial_ constructor
--   which is really type :: Mabye (Address -> Person) 


mkPersonBad :: String -> String -> Maybe Person
mkPersonBad n a = case mkName n of
  Nothing -> Nothing
  Just n' ->
    case mkAddress a of 
      Nothing -> Nothing
      Just a' -> Just $ Person n' a'

-- let s = "old macdonalds'"
-- let addy = mkAddress s
-- let b = mkName "Babe"
-- let person = fmap Person b
-- person <*> addy
-- remember fmap is <$> in infix

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a
-- fmap person n, and then applicative functor it (apply? tie-fighter)


-- instance Functor Maybe where
-- fmap _ Nothing = Nothing
-- fmap f (Just a) = Just (f a)

-- instance Applicative Maybe where
-- pure = Just
-- Nothing <*> _ = Nothing
-- _ <*> Nothing = Nothing
-- Just f <*> Just a = Just (f a)

-- looking at validateLength 25 'babe'  we get Just "babe"
-- looking at mkName "babe" = fmap Name $ Just "babe"
-- fmap Name $ Just "Babe"
-- so the data constructor Name is the function (a ->b) and we are mapping it over some functorial f
-- in this case f is Maybe.  THe a in f a is Sring
-- (a -> b) -> f a -> f b
-- t: Name  :: (String -> Name)
-- t: Just "babe" :: Maybe String

-- type M = Maybe

-- (a -> b)        -> f a        -> f b
-- (string -> name) -> M String  -> M Name

-- since we know we're dealing w/ the Functor instance for Maybe we can inline that function's def too
-- fmap _ Nothing = Nothing
-- fmap (JUst a)  = Just (f a)

-- fmap f (Just a) = Just (f a)
-- fmap Name (Just "Babe") = Just (Name Babe)
-- mkname "babe" = fmap Name $ Just "babe"
-- mkName "babe" = Just (Name "Babe")

-- Maybe applicative and person
---------------------------------------------

-- data Person = Person Name Address deriving (eq, Show)
-- person takes two arguements

-- Person
-- <$> Just (Name "babe")
-- <*> Just (Address "farm")

-- fmap Person (Just (Name "babe"))  will return Maybe (Address -> Person) 
-- :t Person :: Name -> Address -> Person
-- :t Just (Name "babe") :: Maybe Name

-- (a -> b) -> f a -> f b
-- (Name -> Addres -> Person)
--   a -> b
-- Maybe Name -> Maybe (Address -> Person)
--  f      a        f           b

-- fmap _ Nothing = Nothing
-- fmap f (JUst a) = Just (f a)
-- fmap Person (Just (name "Babe"))

-- f :: Person
-- a :: Name "Babe"

-- fmap f (just a) = Just (f a)
-- fmap Person (Just (Name "Babe")) = Just (Person (Name "babe"))
-- the problem is Person (Name "babe") is still waiting for one more arguement.
--    partially applied.  That is our (a -> b) in the applicatives <*>.  the f
--    wrapping this is Maybe which results from us possible not having had an a to map over
--    to begni with, resulting in a Nothing value


-- t: Just (Person(Name "Babe")) :: Maybe (Address -> Person)
-- t: Just (Address "farm") :: Maybe Address  -- we want to get this into the above

-- Just (Person(Name "Babe")) <*> Just (Address "Farm")
-- the function Person(Name "Babe") is inside the f so we want to apply it with <*>



data Cow = Cow {
  name :: String
 , age :: Int
 , weight :: Int
 } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- validating to get rid of empty strings, neg numbers

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just mammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              Just (Cow mammy agey weighty)

--   ^-------- is terrible.

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
  Cow <$> noEmpty name'  -- map Cow (as if it is a function) over Maybe Name  
      <*> noNegative age' -- apply this function to Maybe Int
      <*> noNegative weight' -- apply this function again to maybe int
      
cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
  liftA3 Cow (noEmpty name') (noNegative age') (noNegative weight')
  
-- let cow1 = Cow <$> noEmpty "Bess"
-- :t cow1  will be Maybe (Int -> Int -> Cow)
-- let cow2 = cow1 <*> noNegative 1
-- :t cow2  will be Maybe (Int -> cow)
-- let cow3 = cow2 <*> noNegative 2
-- :t cow3 will be Maybe Cow
-- so you are ... sort of like mapping itself.

-- let cow1 = liftA3 Cow
-- :t cow1 ...  cow1:: Applicative f => f string -> f Int -> f Int -> f Cow
-- let cow2 = cow1 (noEmpty "blah")
-- :t cow2 ... cow 2:: Maybe Int -> Maybe Int -> Maybe Cow
-- let cow3 = cow2 (noNegative 1)
-- :t cow3 ... cow 3 :: Maybe Int -> Maybe Cow
-- let cow4 = cow3 (noNegative 2)
-- :t cow 4 ... cow 4 :: Maybe Cow

-- so applicative is kind of just saying .. we fmap'd my function over some
--    functorial 'f' or it was already in f somehow

-- e..g f ~ maybe
-- cow1 :: Maybe (Int -> Int -> Cow)
-- cow1 = fmap Cow (noEmpty "Bess")
-- and hit a situation where we want to map f (a -> b) NOT JUST (a -> b)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--    over some f a
--    to get an f b
-- cow2 :: Maybe (Int -> Cow)
-- cow2 = cow1 <*> noNegative 1


-- kind of like fmap but we may not have a function at all.


-- f ~ Maybe
-- type M = Maybe
-- maybeApply :: M (a -> b) -> M a -> M b
-- maybeFmap ::    (a -> b) -> M a -> M b

-- fixer upper
-- const <$> Just "Hello" <*> "World"
-- const <$> Just "Hello" <*> Just "World" -- Just Hello

-- (,,,) Just 90 <*> Just 10 Just "Tierness" [1,2,3]
-- (,,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]

-- made it to pg 721 applicative laws
