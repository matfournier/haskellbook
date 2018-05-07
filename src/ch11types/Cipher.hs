module Cipher where
import Data.List
import Data.Char

-- as pattern wen you want to refer to the whole inside a pattern match

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

-- the @ means we can still use the whole tuple

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUP xs@(x:_) = x : xs


isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf a@(x:_) b@(y:_) = and $ foldr (\x acc -> elem x b : acc) [True] a


toUpperWord :: String -> String
toUpperWord (x:xs) = toUpper x : xs

-- capitalizeWords :: String -> [(String, String)]
-- capitalizeWords [] = [] 
-- capitalizeWords s = xs map toUpper
--   where xs = s split ' '  // this doesn't exist ... 
  
  
