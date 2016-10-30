insert :: Ord a => a -> [a] -> [a]
insert y [] = y:[]
insert y (x:xs)
	| (y < x) = y:x:xs
	| (y >= x) =  x:(insert y xs)

insertsort :: Ord a => [a] -> [a]
insertsort [] = []
insertsort (x:xs) = insert x (insertsort xs)