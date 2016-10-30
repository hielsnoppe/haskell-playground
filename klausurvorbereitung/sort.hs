-- Einfaches Beispiel für eine Moduldefinition am Beispiel Sortieralgorithmen
-- November 2007 
-- Leopold Kneidinger

module Sortieren
where


-- Bubblesort
bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs) 
	| x <= y	= x:bubble(y:xs)
	| otherwise	= y:bubble(x:xs)

bubblesort :: Ord a => [a] -> [a]
bubblesort [] = []
bubblesort [x] = [x]
bubblesort xs = bubblesort (init ys) ++ [last ys]
		where ys = bubble xs
	
xs = [2,6,1,9,0,7,5,3,8,4]
abc = ['z', 'f', 'a', 'd']

-- Mergesort
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
		| x < y 	= x:merge xs (y:ys)
		| x > y 	= y:merge (x:xs) ys
		| otherwise = x:merge xs ys
		
-- alternativ mit if-Anweisungen
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] [] = []
merge' xs [] = xs
merge' [] ys = ys
merge' (x:xs) (y:ys) = if (x < y) then x:merge' xs (y:ys)
					   else if (x > y) then y:merge (x:xs) ys
					   else x:merge' xs ys

teilen xs = ((take n xs), (drop n xs))
		where m = length xs
		      n = m `div` 2
			  
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (take n xs)) (mergesort (drop n xs))
				where 
					m = length xs
					n = m `div` 2
					

					

-- Quicksort mit höheren Listenfunktionen			
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (a:as) = qsort [b | b <- as, b < a] ++ [a]
				++ qsort [ b | b <- as, b >= a]		

-- Insertsort
-- Menge wird in zwei Teile geteilt (sortiert und unsortiert)
-- in den sortierten Teil werden die Elemente des unsortierten eingefügt

insert :: Ord a => a -> [a] -> [a]
insert y [] 	= [y]
insert y (x:xs)	
	| (y <= x) = y:x:xs
	| otherwise = x:insert y xs
	
insertsort :: Ord a => [a] -> [a]
insertsort [] = []
insertsort [x] = [x]
insertsort (x:xs) = insert x (insertsort xs)

-- alternativ mit Rechtsfaltung
insertsort' xs = foldr insert [] xs
				
