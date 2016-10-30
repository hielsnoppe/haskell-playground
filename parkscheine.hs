-- Eingabealphabet
--E = [50, 100, 200]
type E = Integer
-- Ausgabealphabet
--aus = ["öffnen", "nichts"]
type A = String
-- Zustandsmenge
--Z = ["z0", "z50", "z100", "z150"]
type Z = String
-- Anfangszustand = z0
-- Endzustand = z0

-- Übergangsfunktion
uber :: E -> Z -> Z

uber 50 "z0" = "z50"
uber 100 "z0" = "z100"
uber 200 "z0" = "z200"

uber 50 "z50" = "z100"
uber 100 "z50" = "z150"
uber 200 "z50" = "z200"

uber 50 "z100" = "z150"
uber 100 "z100" = "z200"
uber 200 "z100" = "z200"

uber 50 "z150" = "z200"
uber 100 "z150" = "z200"
uber 200 "z150" = "z200"

uber _ _ = "nichts"

-- Ausgabefunktion
aus :: E -> Z -> A

aus 50 "z0" = "nichts"
aus 100 "z0" = "nichts"
aus 200 "z0" = "nichts"

aus 50 "z1" = "nichts"
aus 100 "z1" = "nichts"
aus 200 "z1" = "öffnen"

aus 50 "z100" = "nichts"
aus 100 "z100" = "öffnen"
aus 200 "z100" = "öffnen"

aus 50 "z150" = "öffnen"
aus 100 "z150" = "öffnen"
aus 200 "z150" = "öffnen"

aus _ _ = "nichts"

akkuaus :: [E] -> Z -> [A] -> [A]
akkuaus (e:es) z a = akkuaus es (uber e z) ((aus e z):a)
akkuaus [] z a = a

park :: [E] -> [A]
park e = akkuaus e "z0" []