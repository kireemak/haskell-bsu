module BinaryTree where

data Tree a = Empty 
            | Node a (Tree a) (Tree a)

instance Eq a => Eq (Tree a) where
    Empty == Empty = True
    (Node x1 l1 r1) == (Node x2 l2 r2) = (x1 == x2) && (l1 == l2) && (r1 == r2)
    _ == _ = False

instance Show a => Show (Tree a) where
    show Empty = "Empty"
    show (Node x left right) = 
        "(Node " ++ show x ++ " " ++ show left ++ " " ++ show right ++ ")"

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Empty = Empty
treeMap f (Node x left right) = Node (f x) (treeMap f left) (treeMap f right)

treeCount :: Tree a -> Int
treeCount Empty = 0
treeCount (Node _ left right) = 1 + treeCount left + treeCount right

treeTraverseD :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseD f acc Empty = acc
treeTraverseD f acc (Node x left right) = 
    let accLeft = treeTraverseD f acc left
        accCurr = f x accLeft
    in treeTraverseD f accCurr right

treeTraverseW :: (a -> b -> b) -> b -> Tree a -> b
treeTraverseW f acc root = recursiveBFS [root] acc
  where
    recursiveBFS [] currentAcc = currentAcc
    recursiveBFS nodes currentAcc = 
        let 
            vals = getValues nodes
            newAcc = foldl (\ac v -> f v ac) currentAcc vals
            children = getChildren nodes
        in 
            if null children 
            then newAcc 
            else recursiveBFS children newAcc

    getValues [] = []
    getValues (Empty : xs) = getValues xs
    getValues (Node x _ _ : xs) = x : getValues xs

    getChildren [] = []
    getChildren (Empty : xs) = getChildren xs
    getChildren (Node _ l r : xs) = l : r : getChildren xs

testTree :: Tree String
testTree = Node "Eins" 
             (Node "Zwei" (Node "Vier" Empty Empty) (Node "Fuenf" Empty Empty))
             (Node "Drei" (Node "Sechs" Empty Empty) (Node "Sieben" Empty Empty))
             
mainTree :: IO ()
mainTree = do
    putStrLn $ "DFS: " ++ treeTraverseD (\s acc -> acc ++ " " ++ s) "" testTree
    putStrLn $ "BFS: " ++ treeTraverseW (\s acc -> acc ++ " " ++ s) "" testTree