module Ueb06 (lines) where

import Prelude hiding (lines)

-- 1. Aufgabe
-- a)

lines :: [Char] -> [[Char]]
lines ""    = []
lines text  = [fst split] ++ (lines (snd split))
              where split = firstline "" text

firstline :: [Char] -> [Char] -> ([Char], [Char])
firstline line ""           = (line, [])
firstline line ('\n':rest)  = (line, rest)
firstline line (char:[])    = (line ++ [char], [])
firstline line (char:rest)  = firstline (line ++ [char]) rest

-- b)

isPoem :: Int -> [Char] -> Bool
isPoem n text = let suffixes = (map (\ line -> take n (reverse line)) (lines text))
                in all (== head suffixes) (tail suffixes)

-- c) TODO

-- d) TODO (optional)

-- 2. Aufgabe TODO

perfectsUntil :: Int -> [Int]
perfectsUntil n = []

-- 3. Aufgabe
-- a) TODO

-- b) TODO

-- 4. Aufgabe
counts :: (Num a, Eq b) => b -> [b] -> a
counts x list = foldr (+) 0 (map (\ i -> if i == x then 1 else 0) list)
-- 1. Replace matching elements with 1 and others with 0:
--                           map (\ i -> if i == x then 1 else 0) list
-- 2. Sum all elements:
--              foldr (+) 0 (                                         )

single :: (a -> Bool) -> [a] -> Bool
single cond list = foldr (+) 0 (map (\ i -> if (cond i) then 1 else 0) list) == 1
-- 1. Replace satisfying elements with 1 and others with 0:
--                              map (\ i -> if (cond i) then 1 else 0) list
-- 2. Sum all elements:
--                 foldr (+) 0 (                                           )
-- 3. Test whether sum is 1:
--                                                                           == 1

--mostly :: (a -> Bool) -> [a] -> Bool
--okt2bin :: [Char] -> [Char]
--ggt_of :: [Integer] -> Integer
--bin2dec :: [Integer] -> Integer
