-- ADT Listen
-- November 2008

module ListenAlg (Listen, 											-- Typdeklaration
			   initL, cons, einfuegen, entfernen, isEmpty) where	-- exportierte Funktionen

-- -------------------------------------------------------------------
-- initL :: 	Vor.: keine
--          	Eff.: liefert einen neuen leeren Listen
-- -------------------------------------------------------------------
initL	::	Listen a	

-- -------------------------------------------------------------------
-- einfuegen ::     	Vor.: Die Liste ist initialisiert
--             			Eff.: Das Element ist geordnet hinzugefügt
-- -------------------------------------------------------------------
einfuegen:: Ord a => a -> Listen a -> Listen a	

-- -------------------------------------------------------------------
-- cons :: 				Vor.: Keine
--             			Eff.: Das Element wird am Kopf der Liste angehängt.
-- -------------------------------------------------------------------
cons :: a -> Listen a -> Listen a

-- -------------------------------------------------------------------
-- isEmpty :: 			Vor.: Keine
--             			Eff.: Gibt true zurück, falls der Listen leer ist, andernfalls false.
-- -------------------------------------------------------------------
entfernen:: Ord a => a -> Listen a -> Listen a	

-- -------------------------------------------------------------------
-- entfernen :: 		Vor.: Die Liste ist nicht leer.
--             			Eff.: Das Element ist, wenn es vorhanden ist,
--						      aus der Liste entfernt.
-- -------------------------------------------------------------------
isEmpty	:: Listen a	-> Bool

-- -------------------------------------------------------------------
-- Hier beginnt die Implementierung des Datentyps Listen auf der Basis
-- eines algebraischen Datentyps

data Listen a = Empty | Cons a (Listen a) 
				deriving (Show)

initL							= Empty 

cons x Empty  					= Cons x Empty 
cons x xs 						= Cons x xs

einfuegen x Empty  				= Cons x Empty
einfuegen x (Cons a xs) 
		| x < a 				= Cons x (Cons a xs) 
		| x == a				= Cons a xs
		| otherwise 			= cons a (einfuegen x xs)

entfernen x (Cons a xs)			
		| x == a 				= xs
		| otherwise				= cons a (entfernen x xs)
		
isEmpty Empty					= True
isEmpty _						= False


------------------

l0 = initL
l1 = einfuegen 'a' Empty
l2 = isEmpty l0


