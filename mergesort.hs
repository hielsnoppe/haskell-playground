merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
	| (x <= y) = x : merge xs (y:ys)
	| otherwise = y : merge (x:xs) ys

mergesort:: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort ls) (mergesort rs)
	where
		ls = take h xs
		rs = drop h xs
		n = length xs
		h = div n 2