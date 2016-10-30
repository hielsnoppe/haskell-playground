selectionsort :: Ord a => [a] -> [a]
selectionsort x = akkusort x [] where
	akkusort :: Ord a => [a] -> [a] -> [a]
	akkusort [] akku = akku
	akkusort x akku = akkusort (del max x) (max:akku) where
		del :: Eq a => a -> [a] -> [a]
		del x [] = []
		del x (y:ys)
			| (x == y) = ys
			| otherwise = y:(del x ys)
		max = maxi x where
			maxi :: Ord a => [a] -> a
			maxi (x:[]) = x
			maxi (x:y:xs)
				| (x < y) = maxi(y:xs)
				| otherwise = maxi(x:xs)