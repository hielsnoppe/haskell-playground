module List(List, newList) where

data List a = NullL | L a (List a)
	deriving (Show)

newList :: List a
newList = NullL

get :: Int -> List a -> a
get 0 (L x xs) = x
get n NullL = error "Index out of bounds."
get n (L x xs) = get (n-1) xs

cons :: a -> List a -> List a
cons x xs = L x xs

head :: List a -> a
head NullL = error "List empty."
head (L x xs) = x

tail :: List a -> List a
tail NullL = error "List empty."
tail (L x xs) = xs

insert :: Ord a => a -> List a -> List a
insert x NullL = L x NullL
insert x (L y ys)
	| (x < y) = L x (L y ys)
	| (x > y) = L y (insert x ys)
	| (x == y) = L y ys

insertAt :: a -> Int -> List a -> List a
insertAt x 0 NullL = L x NullL
insertAt x 0 (L y ys) = L x (L y ys)
insertAt x n NullL = L x NullL
insertAt x n (L y ys) = L y (insertAt x (n-1) ys)

delete :: Int -> List a -> List a
delete 0 (L x xs) = xs
delete n NullL = NullL
delete n (L x xs) = L x (delete (n-1) xs)

sort :: Ord a => List a -> List a
sort NullL = NullL
sort (L x NullL) = L x NullL
sort xs = insort xs NullL where
	insort :: Ord a => List a -> List a -> List a
	insort NullL ys = ys
	insort (L x xs) ys = insort xs (insert x ys)

l = cons 3 (cons 5 (cons 7 (cons 4 newList)))