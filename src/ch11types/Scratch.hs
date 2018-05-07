module Scratch where


--data Bool = False | True

data Trivial = Trivial'
--    1            2
--    1 the type constructor Trivial is like a constant value, takes no arguements
--    is nullary
--    the data constructor Trivial' is also like a constant value, but it exists in value/
--      runtime space

data UnaryTypeCon a = UnaryValueCon a

--    UnaryTypeCon is the type constructor of one arguement
--      waiting for a type constant to be applied to it
--    UnaryValueCon is the data constructor of one arguement
--      waiting for a value to be applied to it
--      think like a box to put values in

-- the list type
-- data [] a = [] | a : [a]
-- this must be applied to a concrete type before you have a list
--  we see the paralel with fnctions when we look at the KIND signature
--  kinds are the types of types
--    we represent kinds with *
--    we know something is fully applied (concrete) when it's represented with *
--    When it is * -> * it, like a function, is still waiting to be applied

-- let f = not True
-- :t f
-- f :: bool

-- let f x = x > 3
-- :t f
-- f :: (ord a, Num a) => a -> Bool

-- the first f takes no args and is not waiting on application to produce a value
--   it is CONCRETE
--   notice lack of fn arrow

-- second f is awaiting application to an x so it's type sig has a function arrow
-- let f x = x > 3
-- :t f 5
-- f 5 :: Bool   <-- has become concrete

-- haskell makes a disction between type _constants_ and _type constructors_

data PugType = PugData
--        1         2

data HuskyType a = HuskyData
--          3              4

data DogueDeBorduex doge = DogueDeBordeux doge
--          5                     6


--1 pugtype is the type constructor but is a type constant, it enums one constructor
--2 pugdata is the only data construtor for the pugtype, and  happens to be a constant value
--   for any fn that requires a type of PugType you know that value will be pugData

--3 huskytype is the type constructor and it takes a single parameter parametrically polymorphic
--  type varabile as it's arguement, it also enums one data constructor
--4 huskydata is the data constructor.
--    note type param a does NOT occur as an arg to huskydata
--    type argument is phantom
--    is a constant data

--5 DogueDeBordeux is a type constructor that has a single type varable
--6 DogueDebordeux is the lone data constructor, has the same name, but is NOT the same thing
--  the doge type varaible occurs in both
--  because they are the same type varaible they must agree with each other
--  e.g. if you have a DOgueDeBordex[Person] you must necessarily have a list of Person values containt
--  in the DOgueDebordeux value
--  MUST be applied before it's concrete value, it's literal value can change at run time

-- this works
-- myDoge :: DogueDebordeux Int
-- myDoge = DogueDeBordeux 10

-- but myDoge = DogueDeBordeux "10" wouldn't

data Doggies a =
  Husky a | Mastiff a
  deriving (Eq, Show)


data Price = Price Integer deriving (Eq, Show)
--  price has one type constructor, one data constructor, and one arguement in data const
-- the value prce depends on the type Integer

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
-- manufactor has _three_ data constructors

data Airline = Papuair | AC | United deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline deriving(Eq, Show)
--      k        l    2             3      m       4
-- k = type constrcutor
-- l and m - two different data constructors
-- k takes 2 and 3 type arguments
-- m taskes 4 as a type arguement

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
doge = Plane Papuair

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

areAllCars :: [Vehicle ] -> Bool
areAllCars = all isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
-- but will explode if you pass it an airplane, so possible option?

-- 11.7 data constructor arities
-------------------------------------------------------------------------------
-- arity => # of args a function or construction takes
-- no args => nullary
-- data constructors such as True and False are constant values
--    they take no args, they can't construct or represent any other data than themselves

-- nullary
data Example0 = Example0 deriving (Eq, Show)

-- unary
data Example1 = Example1 Int deriving (Eq, Show)

-- product of Int and String
data Example2 = Example2 Int String deriving (Eq, Show)

data MyType = MyVal Int deriving (Eq, Show)
--    [1]     [2]    [3]
-- 1 type constructor
-- 2 data constructor takes one type arg, is called unary
-- the type arguement to [2]

-- cardinality exercises
-- 1. data pugtype = pugdata  > 1
-- 2. data ariline = a | b |c   > 3
-- 3:
-- 4

data Example = MakeExample deriving Show

data IntExampple = MakeIntExample Int deriving Show


-- newtype
--   cannot be a product type, sum type, or contain a nullary constrcutor
--   has no runtime overhead


-- say we had

--tooManyGoats :: Int -> Bool
--tooManyGoats n = n > 42

-- but what if we mixed up the # of cows we had with goats? and they had dif limits?

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42
-- if you pass in something else, won't compile

-- newtype can also do typesnyoms..

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 50

-- even though goats is basically a int, it can be defined differently than the underling
-- int in the typeclass...



