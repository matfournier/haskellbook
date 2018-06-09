module Ziplist where

-- default monoid of lists in ghc prelude is concatenation
-- but there is another way to combine lists
-- whereas the default list mappend ends up doing the following

-- [1,2,3] <> [4, 5, 6]
-- changes to [1,2,3] ++ [4,5,6]
-- [1,2,3,4,5,6]

-- we could also do [1,2,3] <> [4,5,6]
-- to become [1 <> 4, 2 <> 5, 3 <> 6]

import Data.Monoid

-- 1 <> 2 -- will throw

-- skipped
