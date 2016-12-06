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
-- a)

pack :: (Eq a) => [a] -> [(a, Int)]
pack []     = []
pack [x]    = [(x, 1)]
pack (x:xs) = packr (x, 1) xs

packr :: (Eq a) => (a, Int) -> [a] -> [(a, Int)]
packr tup []    = [tup]
packr (e, n) (x:xs)
    | (e == x)  = packr (e, n + 1) xs
    | otherwise = [(e, n)] ++ (packr (x, 1) xs)

-- b)

pack2 :: (Eq a) => [a] -> [(a, Int)]
pack2 list  = foldr packr2 [(last list, 1)] (init list)

packr2 :: (Eq a) => a -> [(a, Int)] -> [(a, Int)]
packr2 x []     = [(x, 1)]
packr2 x ((e, n):rest)
    | (e == x)  = (e, n + 1) : rest
    | otherwise = (x, 1) : (e, n) : rest

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

mostly :: (a -> Bool) -> [a] -> Bool
mostly cond list = foldr (+) 0 (map (\ i -> if (cond i) then 1 else -1) list) > 0
-- 1. Replace satisfying elements with 1 and others with -1:
--                              map (\ i -> if (cond i) then 1 else -1) list
-- 2. Sum all elements:
--                 foldr (+) 0 (                                            )
-- 3. Test whether sum is greater than 0:
--                                                                            > 0

--okt2bin :: [Char] -> [Char]
--ggt_of :: [Integer] -> Integer
--bin2dec :: [Integer] -> Integer
