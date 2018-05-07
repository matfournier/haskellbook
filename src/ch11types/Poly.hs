-- LISTS

-- data [] a = [] | a : [a]
--      1  2   3    4 5  6

-- 1 the type constructor for lists has a special [] syntax
-- 2 single type arguement [].  This is the type of value our list contains
-- 3 nil/empty
-- 4 single value of type a
-- 5 : is an infix data constructor (it is product of a [4] and [a] 6)
-- rest of our list

-- infix type and data constructors
--   when we give an operator a non-alphanumeric name, it is infix by default
-- any operator that starts with a (:) must be an infix type or data constructor

-- list w/o infix
-- data List a = Nil | Cons a (List a)
-- looks more like the scala version ...


data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- this tree has a value of type a at each node
-- each node could be a terminal node (leaf) or a branch w/ two subtrees

-- inserting into tree... we need ORD!
--  if something is lower, go left
--  if something is higher, go right

-- insert will insert a val into a tree
--  if NO tree exists yet, it gives us a means of building a tree by inserting values
--  note: we are building a whole new tree each time here :(

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)                                      

 -- Leaf being the tmpy tree case
  -- let t1 = insert' 0 leaf
  -- t1 Node Leaf 0 Leaf

  -- let t2 = insert' 3 t1
  -- Node Leaf 0 (Node leaf 3 Leaf)

  -- let t3 = insert' 5 t2 
  -- t3
  -- Node Leaf 0
  --  (No Leaf 3
  --    (Node Leaf 5 Leaf))

-- map for binary tree

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

-- testTree' :: BinaryTree Integer
-- testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
-- mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- -- acceptance test for mapTree
-- mapOkay = if mapTree (+1) testTree' == mapExpected
--   then print "yup okay!"
  -- else error "test failed!"


-- convert binary tree to a list

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node Leaf a Leaf) = [a]
preorder (Node l x r) = x : (preorder l) ++ (preorder r) 

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node Leaf a Leaf) = [a]
inorder (Node l x r) = (inorder l) ++ [x] ++ (preorder r)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node Leaf a Leaf) = [a]
postorder (Node l x r) = (postorder l) ++ (postorder r) ++ [x]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
  2
  (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn("preorder pass")
  else putStrLn("preorder fail")

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

-- writing fold

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc (Node l a r) =
  let left = foldTree f acc l
      right = foldTree f left r
      current = f a right
  in current
foldtree _ acc Leaf = acc 


