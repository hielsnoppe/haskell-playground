bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs)
	| (x > y) = y : bubble (x:xs)

bubblesort :: Ord a => [a] -> [a]
bubblesort [] = []
bubblesort (x:xs) = bubblesort (init ys) ++ [last ys]
	where ys = bubble (x:xs)