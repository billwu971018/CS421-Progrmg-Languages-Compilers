data Tuple a b = Tuple a b
  deriving Show

tuple2pair :: Tuple a b -> (a,b)
tuple2pair (Tuple a b) = (a, b)
tuple2pair (Tuple a (Tuple bb cc)) = (a, tuple2pair(Tuple bb cc))
tuple2pair (Tuple (Tuple aa bb) c) = (tuple2pair (Tuple aa bb), c)
tuple2pair (Tuple (Tuple aa bb) (Tuple cc dd)) = (tuple2pair (Tuple aa bb), tuple2pair (Tuple cc dd)) 

prodTuple :: Tuple Integer Integer -> Integer
prodTuple (Tuple a b) = a*b
prodTuple (Tuple a (Tuple bb cc)) = (a, prodTuple(Tuple bb cc))
prodTuple (Tuple (Tuple aa bb) c) = (prodTuple (Tuple aa bb), c)
prodTuple (Tuple (Tuple aa bb) (Tuple cc dd)) = (prodTuple (Tuple aa bb), prodTuple (Tuple cc dd)) 