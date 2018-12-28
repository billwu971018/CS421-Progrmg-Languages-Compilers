flipzip :: [a] -> [b] -> [(b, a)]
flipzip _ _ =[]
flipzip [] _ = []
flipzip _ [] = []
flipzip (x:xs) (y:ys) = (y,x): flipzip xs ys