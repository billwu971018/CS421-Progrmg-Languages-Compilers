 minlist :: (Ord a) => [a] -> a 
 minlist [] a = a
 minlist (x:xs) a 
    | x<a = minlist xs x
    |otherwise = minlist xs a