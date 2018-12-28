doubleEvens :: [Integer] -> [Integer]
doubleEvens xx = map (\v->v*2) (filter (even) xx)