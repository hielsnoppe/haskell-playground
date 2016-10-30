-- ADT Listen
-- November 2008

module Listen (Listen, 									-- Typdeklaration
			   initL, einfuegen, cons, isEmpty) where	-- exportierte Funktionen

-- -------------------------------------------------------------------
-- initL :: 	Vor.: keine
--          	Eff.: liefert einen neuen leeren Listen
-- -------------------------------------------------------------------
initL	::	Listen a	

-- -------------------------------------------------------------------
-- einfuegen ::     Vor.: Die Liste ist initialisiert
--             		Eff.: Das Element ist geordnet hinzugefügt
-- -------------------------------------------------------------------
einfuegen:: Ord a => Listen a -> a -> Listen a	

-- -------------------------------------------------------------------
-- cons 	:: Vor.: Keine
--             Eff.: Das Element wird am Kopf der Liste angehängt.
-- -------------------------------------------------------------------
cons :: Listen a -> a -> Listen a

-- -------------------------------------------------------------------
-- isEmpty 	:: Vor.: Keine
--             Eff.: Gibt true zurück, falls der Listen leer ist, andernfalls false.
-- -------------------------------------------------------------------
isEmpty				:: Listen a	-> Bool

-- -------------------------------------------------------------------

-- Hier beginnt die Implementierung des Datentyps Listen auf der Basis
-- des eingebauten Listenmoduls

data Listen a = Listen [a]
				deriving (Show)

initL						= Listen [] 

einfuegen (Listen  []) x 	= Listen [x]
einfuegen (Listen (y:xs)) x
		| x < y 			= (Listen (x:y:xs)) 
		| x == y			= Listen (y:xs)
		| otherwise 		= cons (einfuegen (Listen (xs)) x) y

cons (Listen []) x 			= Listen (x:[])
cons (Listen xs) x 			= Listen (x:xs)

isEmpty (Listen [])			= True



