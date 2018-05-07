module High where

data Silly a b c d =
  MkSilly a b c d deriving Show

-- Prelude> :kind Silly
-- Silly :: * -> * -> * -> * -> *
-- Prelude> :kind Silly Int
-- Silly Int :: * -> * -> * -> *
-- Prelude> :kind Silly Int String
-- Silly Int String :: * -> * -> *
-- Prelude> :kind Silly Int String Bool
-- Silly Int String Bool :: * -> *
-- Prelude> :kind Silly Int String Bool String
-- Silly Int String Bool String :: *

-- Prelude> :kind (Int, String, Bool, String)
-- (Int, String, Bool, String) :: *

-- data EsResultFound a =
--   EsResultFound { _version :: DocVersion
--                 , _source :: a }
--   deriving (Eq, Show)
 
