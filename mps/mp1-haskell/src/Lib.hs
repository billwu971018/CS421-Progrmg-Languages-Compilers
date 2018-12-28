--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake n _  
    | n <= 0   = []  
mytake _ []     = []  
mytake n (x:xs) = x : mytake (n-1) xs  

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n (x:xs) 
      | n <= 0 = x:xs
      | otherwise = mydrop (n-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev xx = aux xx []
      where aux [] acc = acc
            aux (x:xs) acc = aux xs (x:acc)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app xs [] = xs
app [] xs = xs
app (x:xs) y = x: app xs y

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1): inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a 
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x,y): myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs _ [] = []
addpairs [] _ = []
addpairs xx yy  = [(x+y) | (x,y)<-acc]
            where acc = myzip xx yy

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1:ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer] 
fib = 0:1: addpairs (fib) (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add a []  = [a]
add a xx@(x:xs) 
    | a < x = a:xx
    | a > x = x: add a xs
    | otherwise = xx

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union x1 [] = x1
union [] x1 = x1
union xx@(x:xs) yy@(y:ys) 
    | x < y = x: union xs yy
    | x > y = y: union xx ys
    | otherwise = x: union xs ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] [] = []
intersect _ [] = []
intersect [] _ = []
intersect xx@(x:xs) yy@(y:ys) 
    | x < y = intersect xs yy
    | x > y = intersect xx ys
    | otherwise = x: intersect xs ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union (map (add x) (powerset xs)) (powerset xs)

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a] 
inclist' xs = map (\x -> (x + 1)) xs

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' xx = foldr (\x y -> (x + y)) 0 xx

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a] 
cons2list Nil = []
cons2list (Cons x xs) = x: cons2list(xs)

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer 
eval (IntExp n) = n 
eval (PlusExp xs) = sum $ map eval xs
eval (MultExp xs) = foldr (*) 1 $ map eval xs

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' xs = foldr Cons Nil xs

--- ### BinTree
data BinTree a = Node a (BinTree a) (BinTree a)
            | Leaf
     deriving (Show, Eq)
-- BinTree

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node n a b)  = n + (sumTree a) + (sumTree b)

--- ### SimpVal
data SimpVal = IntVal Integer
         | BoolVal Bool
         | StrVal String
         | ExnVal String
  deriving (Show, Eq)
-- SimpVal

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp (op) (IntVal a) (IntVal b) = IntVal (op a b)
liftIntOp _ _ _ = ExnVal "not an IntVal!"
