-- --------------------------------------------------
-- Modul Vektorrechnung
-- --------------------------------------------------

module Vektorrechnung () where

class VBO a where -- VBO = Vektor basierte Objekte
	kollinear :: VBO a => a -> a -> Bool
	(||?) :: VBO a => a -> a -> Bool



-- --------------------------------------------------
-- Vektoren
-- --------------------------------------------------
data Vektor = V Float Float Float deriving (Show)

-- nV :: Initialisirung eines Vektors
nV :: Float -> Float -> Float -> Vektor
nV x y z = V x y z

-- betrag :: Betrag eines Vektors
betrag :: Vektor -> Float
betrag (V x y z) = sqrt (x*x + y*y + z*z)

-- normiert :: Normiert einen Vektor
normiert :: Vektor -> Vektor
normiert (V x y z) = V (x/b) (y/b) (z/b) where
	b = betrag (V x y z)

-- kreuzprodukt :: Kreuzprodukt zweier Vektoren
kreuzprodukt :: Vektor -> Vektor -> Vektor
kreuzprodukt (V xa ya za) (V xb yb zb) = V (ya*zb - za*yb) (za*xb - xa*zb) (xa*yb - ya*xb)

-- (#) :: Kreuzprodukt zweier Vektoren als Operator
(#) :: Vektor -> Vektor -> Vektor
(#) va vb = kreuzprodukt va vb

-- skalarprodukt :: Skalarprodukt zweier Vektoren
skalarprodukt :: Vektor -> Vektor -> Float
skalarprodukt (V xa ya za) (V xb yb zb) = xa*xb + ya*yb + za*zb

-- (**) :: Skalarprodukt zweier Vektoren als Operator
(**) :: Vektor -> Vektor -> Float
(**) va vb = skalarprodukt va vb
	
instance VBO Vektor where

-- kollinear :: Überprüft die Kollinearität zweier Vektoren
	kollinear (V xa ya za) (V xb yb zb)
		| (xa/xb == ya/yb && xa/xb == za/zb) = True
		| otherwise = False

-- (||?) :: Überprüft die Kollinearität zweier Vektoren
	(||?) va vb = kollinear va vb

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- TODO
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--instance Num Vektor where

-- (*) :: Multiplikation eines vektors mit irgendetwas
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



-- --------------------------------------------------
-- Geraden
-- --------------------------------------------------
data Gerade = G Vektor Vektor deriving (Show)

-- nG :: Initialisierung einer Geraden
nG :: Vektor -> Vektor -> Gerade
nG vs vr = G vs vr

instance VBO Gerade where

-- kollinear :: Überprüft die Kollinearität zweier Geraden
	kollinear (G vsa vra) (G vsb vrb) = kollinear vra vrb
-- (||?) :: Überprüft die Kollinearität zweier Geraden
	(G vsa vra) ||? (G vsb vrb) = kollinear vra vrb



-- --------------------------------------------------
-- Ebenen
-- --------------------------------------------------
data Ebene = EParam Vektor Vektor Vektor | ENorm Vektor Vektor
	deriving (Show)

-- nEParam :: Initialisierung einer Ebene in Parameterform
nEParam :: Vektor -> Vektor -> Vektor -> Ebene
nEParam vs vrs vrt = EParam vs vrs vrt

-- nENorm :: Initialisierung einer Ebene in Normalenform
nENorm :: Vektor -> Vektor -> Ebene
nENorm vs vn = ENorm vs vn

-- normalenform :: Ebene in Normalenform
normalenform :: Ebene -> Ebene
normalenform (EParam vs vrs vrt) = (ENorm vs (vrs # vrt))

-- parameterform :: Ebene in Parameterform
parameterform :: Ebene -> Ebene
parameterform (ENorm vs (V xn yn zn)) = (EParam vs vrs vrt) where
	vrs = nV 1 1 ((-1)*(xn+yn)/zn)
	vrt = (V xn yn zn) # vrs

instance VBO Ebene where

-- (||?) :: Überprüft die Parallelität zweier Ebenen
	(EParam vsa vrsa vrta) ||? (EParam vsb vrsb vrtb) = kollinear (vrsa # vrta) (vrsb # vrtb)
	(ENorm vsa vna) ||? (EParam vsb vrsb vrtb) = kollinear vna (vrsb # vrtb)
	(EParam vsa vrsa vrta) ||? (ENorm vsb vnb) = kollinear (vrsa # vrta) vnb



-- Test-Objekte
va = nV 1 2 3
vb = nV 3 2 1
vc = nV 2 4 6
vd = nV 6 3 1

ea = nEParam vc va vb
eb = nEParam vd va vb
ec = nENorm va vb