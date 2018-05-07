module Io where

getInt :: IO Int
getInt = fmap read getLine

-- could also do (+1) <$> getInt


-- fmap (++ "and me too!") getLine
-- could also do (++ " and me too!") <$> getLine 

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ "and me too!")

-- is the same under the hood..
