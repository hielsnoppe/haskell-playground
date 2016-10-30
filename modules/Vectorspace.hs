-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- TODO
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- --------------------------------------------------
-- Module Vectorspace
-- --------------------------------------------------

module Vectorspace () where

class VBO a where -- VBO = Vector basierte Objekte
	kollinear :: VBO a => a -> a -> Bool
	(||?) :: VBO a => a -> a -> Bool



-- --------------------------------------------------
-- Vectors
-- --------------------------------------------------
data Vector = V Float Float Float deriving (Show)

-- nV :: Initialisirung eines Vectors
nV :: Float -> Float -> Float -> Vector
nV x y z = V x y z

-- betrag :: Betrag eines Vectors
norm :: Vector -> Float
norm (V x y z) = sqrt (x*x + y*y + z*z)

-- normiert :: Normiert einen Vector
normiert :: Vector -> Vector
normiert (V x y z) = V (x/b) (y/b) (z/b) where
	b = betrag (V x y z)

-- kreuzprodukt :: Kreuzprodukt zweier Vectoren
kreuzprodukt :: Vector -> Vector -> Vector
kreuzprodukt (V xa ya za) (V xb yb zb) = V (ya*zb - za*yb) (za*xb - xa*zb) (xa*yb - ya*xb)

-- (#) :: Kreuzprodukt zweier Vectoren als Operator
(#) :: Vector -> Vector -> Vector
(#) va vb = kreuzprodukt va vb

-- skalarprodukt :: Skalarprodukt zweier Vectoren
skalarprodukt :: Vector -> Vector -> Float
skalarprodukt (V xa ya za) (V xb yb zb) = xa*xb + ya*yb + za*zb

-- (**) :: Skalarprodukt zweier Vectoren als Operator
(**) :: Vector -> Vector -> Float
(**) va vb = skalarprodukt va vb
	
instance VBO Vector where

-- kollinear :: Überprüft die Kollinearität zweier Vectoren
	kollinear (V xa ya za) (V xb yb zb)
		| (xa/xb == ya/yb && xa/xb == za/zb) = True
		| otherwise = False

-- (||?) :: Überprüft die Kollinearität zweier Vectoren
	(||?) va vb = kollinear va vb

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- TODO
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--instance Num Vector where

-- (*) :: Multiplikation eines Vectors mit irgendetwas
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



-- --------------------------------------------------
-- Linen
-- --------------------------------------------------
data Line = G Vector Vector deriving (Show)

-- nG :: Initialisierung einer Linen
nG :: Vector -> Vector -> Line
nG vs vr = G vs vr

instance VBO Line where

-- kollinear :: Überprüft die Kollinearität zweier Linen
	kollinear (G vsa vra) (G vsb vrb) = kollinear vra vrb
-- (||?) :: Überprüft die Kollinearität zweier Linen
	(G vsa vra) ||? (G vsb vrb) = kollinear vra vrb



-- --------------------------------------------------
-- Plainn
-- --------------------------------------------------
data Plain = EParam Vector Vector Vector | ENorm Vector Vector
	deriving (Show)

-- nEParam :: Initialisierung einer Plain in Parameterform
nEParam :: Vector -> Vector -> Vector -> Plain
nEParam vs vrs vrt = EParam vs vrs vrt

-- nENorm :: Initialisierung einer Plain in Normalenform
nENorm :: Vector -> Vector -> Plain
nENorm vs vn = ENorm vs vn

-- normalenform :: Plain in Normalenform
normalenform :: Plain -> Plain
normalenform (EParam vs vrs vrt) = (ENorm vs (vrs # vrt))

-- parameterform :: Plain in Parameterform
parameterform :: Plain -> Plain
parameterform (ENorm vs (V xn yn zn)) = (EParam vs vrs vrt) where
	vrs = nV 1 1 ((-1)*(xn+yn)/zn)
	vrt = (V xn yn zn) # vrs

instance VBO Plain where

-- (||?) :: Überprüft die Parallelität zweier Plainn
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