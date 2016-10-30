module BinTree (BinTree, newBinTree, insert, insertAll, isEmpty, delete, inorder, sum, height) where

data BinTree a = NullT | K (BinTree a) a (BinTree a)
	deriving (Show)

newBinTree :: BinTree a
newBinTree = NullT

insert :: Ord a => a -> BinTree a -> BinTree a
insert x NullT = (K NullT x NullT)
insert x (K yl y yr)
	| (x < y) = (K (insert x yl) y yr)
	| (x > y) = (K yl y (insert x yr))
	| otherwise = (K yl y yr)

insertAll :: Ord a => [a] -> BinTree a -> BinTree a
insertAll [] tree = tree
insertAll (x:[]) tree = insert x tree
insertAll (x:xs) tree = insertAll xs (insert x tree)

isEmpty :: BinTree a -> Bool
isEmpty NullT = True
isEmpty (K xl x xr) = False

delete :: Ord a => a -> BinTree a -> BinTree a
delete x NullT = NullT -- Baum ist leer
delete x (K NullT y NullT) -- Baum ist ein Blatt
	| (x == y) = NullT -- x war Blatt
	| otherwise = K NullT y NullT -- x nicht im Baum
delete x (K NullT y yr) -- linker Teilbaum ist leer
	| (x < y) = K NullT y yr -- x nicht im Baum
	| (x > y) = K NullT y (delete x yr) -- x im rechten Ast
	| (x == y) = K NullT sym (delete sym yr) where -- x gefunden
			sym = smallest yr where
				smallest :: BinTree a -> a
				smallest NullT = error "Tree empty."
				smallest (K NullT y yr) = y
				smallest (K yl y yr) = smallest yl
delete x (K yl y NullT) -- rechter Teilbaum ist leer
	| (x < y) = K (delete x yl) y NullT -- x im linken Ast
	| (x > y) = K yl y NullT -- x nicht im Baum
	| (x == y) = K (delete sym yl) sym NullT where -- x gefunden
			sym = biggest yl where
				biggest :: BinTree a -> a
				biggest NullT = error "Tree empty."
				biggest (K yl y NullT) = y
				biggest (K yl y yr) = biggest yr
delete x (K yl y (K rl r rr)) -- Baum ist eine normale Astgabel
	| (x < y) = K (delete x yl) y (K rl r rr) -- x im linken Ast
	| (x > y) = K yl y (delete x (K rl r rr)) -- x im rechten Ast
	| (x == y) = K (delete sym yl) sym (K rl r rr) where -- x gefunden
			sym = biggest yl where
				biggest :: BinTree a -> a
				biggest NullT = error "Tree empty."
				biggest (K yl y NullT) = y
				biggest (K yl y yr) = biggest yr

inorder :: BinTree a -> [a]
inorder NullT = []
inorder (K yl y yr) = (inorder yl) ++ y:(inorder yr)

sumTree :: Num a => BinTree a -> a
sumTree tree = foldr (+) 0 (inorder tree)

height :: BinTree a -> Int
height tree = akkuHeight tree 0 where
	akkuHeight :: BinTree a -> Int -> Int
	akkuHeight NullT akku = akku
	akkuHeight (K yl y yr) akku
		| (ls >= rs) = ls
		| (ls < rs) = rs where
			ls = akkuHeight yl (akku+1)
			rs = akkuHeight yr (akku+1)

t = insert 1 (insert 2 (insert 9 (insert 5 newBinTree)))
t2 = insertAll [9,6,7,8,3,4,5,1,2] t