mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile p (x:xs)
    | p x == True = x:mytakeWhile p xs
    |otherwise = mytakeWhile p xs
