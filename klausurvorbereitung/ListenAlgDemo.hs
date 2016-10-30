-- ADT Listen
-- November 2008

module ListenAlgDemo where	
import ListenAlg


------------------

l0 = initL
l1 = einfuegen 2 l0
l2 = einfuegen 3(einfuegen 2 l1)
l3 = einfuegen 5 (einfuegen 10 (einfuegen 2 l2))

l4 = entfernen 10 l3





