data BinaryTree a = Leaf
    | Node (BinaryTree a) a (BinaryTree a) 
    deriving (Eq, Ord, Show)



insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
        | b == a = Node left a right
        | b < a = Node (insert' b left) a right 
        | b > a = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = 
    Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = 
    a : (preorder left) ++ (preorder right)


testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf) 2
    (Node Leaf 3 Leaf)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = 
    f a (foldTree f (foldTree f b right) left)