module Sum where


-- data Bool = False | True

-- two data constructors each representing one possible values

data BigSmall =
  Big Bool
  | Small Bool
  deriving (Eq, Show)


data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth
                 deriving (Eq, Show)


data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

-- could have also been written as type TwoQs = (QuantumBool, QuantumBool)

-- RECORD SYNTAX

data Person =
  MkPerson String Int
  deriving (Eq, Show)

data PersonR =
  Person { name :: String
         , age :: Int }
             deriving (Eq, Show)


-- CONSTRUCTING AND DECONSTRUCTING VALUES

-- with a value
--   we can generate or constrct it
--   we can match on it and consume it

data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
                deriving (Eq, Show)
  
-- ??

trivialValue :: GuessWhat
trivialValue = Chickenbutt

-- we define trivialValue to be the nullary data constructor Chickebutt
-- we have a value of type Guesswhat


idInt :: Id Integer
idInt = MkId 10 
