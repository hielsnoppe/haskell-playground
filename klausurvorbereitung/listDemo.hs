module ListenDemo where
import Listen

-- -------------------------------------------------------------------
-- Beispiel Listen
-- -------------------------------------------------------------------
l0 = initL
l1 = einfuegen initL 'O'
l2 = einfuegen(einfuegen(einfuegen (einfuegen l1 'L')'L')'A')'H' 

c0 = initL
c1 = cons initL 'O'
c2 = cons(cons(cons(cons c1 'L')'L')'A')'H' 

