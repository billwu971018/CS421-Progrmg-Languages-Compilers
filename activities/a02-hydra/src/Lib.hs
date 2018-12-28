module Lib
    (chop) where


-- Your code here!
-- Feel free to write helper functions.

-- chop :: [Int] -> [Int]

chop :: [Int] -> [Int]
chop input 
     | (maximum input == 0) = input
     | otherwise = Hydra input (length input)

Hydra (x:xs) level 
 | x > 0 = (x - 1) : (replaceNth 0 (head xs + level - 1) xs) 
 | x == 0 = x : Hydra xs (level - 1)
 |otherwise = replicate ((length xs) + 1) 0


replaceNth n temp [] = []
replaceNth (x:xs) n temp 
     | n == 0 = temp:xs
     | otherwise = x:replaceNth (n-1) temp xs
