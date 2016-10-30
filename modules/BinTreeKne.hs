-- ADT BinTree mittels algebraischen Datentyps
-- November 2008

module BinTree (BinTree, -- Typ
			  initT) where

data BinTree a = Null | K (BinTree a) a (BinTree a) 
				 -- deriving (Show)
				 
-- binärer Baum mit der Ordnung, dass die Elemente im linken Teilbaum
-- kleiner sind als die Elemente im rechten Teilbaum
			   
-- -------------------------------------------------------------------
-- initT :: 	Vor.: keine
--          	Eff.: liefert einen neuen leeren Baum
-- -------------------------------------------------------------------
initT :: a -> BinTree a
initT a	= K Null a Null 			

-- -------------------------------------------------------------------
-- einfuegen 		::     Vor.: Baum ist initialisiert
--             		Eff.:  ein Knoten mit der Wurzel (Knoten) a ist erzeugt
-- -------------------------------------------------------------------
einfuegen :: Ord a => a -> BinTree a -> BinTree a
einfuegen x Null = K Null x Null
einfuegen x (K yl y yr)
	| (x < y) = K (einfuegen x yl) y yr
	| (x > y) = K yl y (einfuegen x yr)
	| otherwise = error "Den Wert gibt's schon!"

-- -------------------------------------------------------------------
-- inorder 			Vor.: Baum ist initialisiert
--             		Eff.:  Gibt den Baum Inorder aus
-- -------------------------------------------------------------------
inorder :: BinTree a -> [a]
inorder Null = []
inorder (K xl x xr) = (inorder xl) ++ (x:(inorder xr))

-- -------------------------------------------------------------------
-- isEmpty 			Vor.: Baum ist initialisiert
--             		Eff.: Ist der Baum leer, wird true zurückgegeben,
--						  andernfalls false.
-- -------------------------------------------------------------------
isEmpty :: BinTree a -> Bool
isEmpty Null = True
isEmpty (K xl x xr) = False

-- -------------------------------------------------------------------
-- minTree 			Vor.: Baum ist initialisiert
--             		Eff.: Gibt das kleinste Element im Baum zurück.
-- -------------------------------------------------------------------
minTree :: BinTree a -> a
minTree (K Null x Null) = x
minTree (K xl x xr) = minTree xl

-- -------------------------------------------------------------------
-- maxTree 			Vor.: Baum ist initialisiert
--             		Eff.: Gibt das größte Element im Baum zurück.
-- -------------------------------------------------------------------
maxTree :: BinTree a -> a
maxTree (K Null x Null) = x
maxTree (K xl x xr) = maxTree xr

-- -------------------------------------------------------------------
-- entfernen 		Vor.: Baum ist initialisiert
--             		Eff.: Die zu löschende Wurzel ist aus dem Baum 
--					      entfernt.
-- -------------------------------------------------------------------
entfernen :: Ord a => a -> BinTree a -> BinTree a
entfernen x Null = Null -- Entfernen aus leerem Baum
entfernen x (K Null y Null) -- Entfernen eines Blattes
	| (x == y) = Null
	| otherwise = (K Null y Null)
enfernen x (K yl y yr) -- Entfernen eines Knotens mit Kindern
	| (x < y) = (K (entfernen x yl) y yr)
	| (x > y) = (K yl y (entfernen x yr))
	| (x == y && isEmpty yl) = (K yl symr yrs)
--		syml = minTree yr
--		yr = entfernen sym yr
	| (x == y && isEmpty yr) = (K yls syml yr)
		where
			symr = maxTree yl -- symmetrischer Nachfolger
			syml = minTree yr -- symmetrischer Vorgänger
			yrs = entfernen symr yr -- rechter Teilbaum ohne symr
			yls = entfernen syml yl -- linker Teilbaum ohne syml

-- showFunktion nach dem Muster: (([]3[])5(([]7[])10([]15[])))
-- -------------------------------------------------------------------
instance (Show a) => Show (BinTree a) where
	show Null = "[]"
	show (K l w r) = "(" ++ (show l) ++ (show w) ++ (show r) ++ ")"
----------------------------------------------------------------------


a = initT 5
b = einfuegen 3 a
c = einfuegen 7 b
d = einfuegen 12 c