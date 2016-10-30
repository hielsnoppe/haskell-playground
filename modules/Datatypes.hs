module Datatypes (Stack, Queue, List, BinTree) where

-- Stack

data Stack a = NullS | S a (Stack a)

newStack :: Stack a
newStack = NullS

top :: Stack a -> a
top NullS = error "Stack empty."
top (S x xs) = x

pop :: Stack a -> Stack a
pop NullS = NullS
pop (S x xs) = xs

push :: a -> Stack a -> Stack a
push x NullS = (S x NullS)
push x xs = (S x xs)

-- Queue

data Queue a = NullQ | Q a (Queue a)

newQueue :: Queue a
newQueue = NullQ

front :: Queue a -> a
front NullQ = error "Queue empty."
front (Q x xs) = x

leave :: Queue a -> Queue a
leave NullQ = NullQ
leave (Q x xs) = xs

enter :: a -> Queue a -> Queue a
enter x NullQ = Q x NullQ
enter x (Q y ys) = Q y (enter x ys)

-- List

data List a = NullL | L a (List a)

newList :: List a
newList = NullL

get :: Int -> List a -> a
get 0 (L x xs) = x
get n NullL = error "Index out of bounds."
get n (L x xs) = get (n-1) xs

insert :: a -> Int -> List a -> List a
insert x 0 NullL = L x NullL
insert x 0 (L y ys) = L x (L y ys)
insert x n NullL = L x NullL
insert x n (L y ys) = L y (insert x (n-1) ys)

delete :: Int -> List a -> List a
delete 0 (L x xs) = xs
delete n NullL = NullL
delete n (L x xs) = L x (delete (n-1) xs)

-- BinTree

data BinTree a = NullT | K (BinTree a) a (BinTree a)

newBinTree :: BinTree a
newBinTree = NullT

insert :: Ord a => a -> BinTree a -> BinTree a
insert x NullT = (K NullT x NullT)
insert x (K yl y yr)
	| (x < y) = (K (insert x yl) y yr)
	| (x > y) = (K yl y (insert x yr))
	| otherwise = (K yl y yr)

isEmpty :: BinTree a -> Bool
isEmpty NullT = True
isEmpty (K xl x xr) = False

--delete :: a -> BinTree a -> BinTree a