{-# LANGUAGE FlexibleContexts #-}

module Scheme.Runtime where

import Scheme.Core
import Scheme.Parse
import Scheme.Eval

import qualified Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable

--- ### Helper functions for lifting and lowering

lowerBool :: Val -> Bool
lowerBool (Boolean False) = False
lowerBool _ = True

lowerInt :: Val -> EvalState Int
lowerInt (Number i) = return i
lowerInt v = throwError $ TypeError v

lowerList :: Val -> EvalState [Val]
lowerList (List xx) = return xx
lowerList v = throwError $ TypeError v

liftIntVargOp :: (Int -> Int -> Int) -> Int -> Val
liftIntVargOp f c = PrimFunc p where
  p [] = return $ Number c
  p [x] = Number . f c <$> lowerInt x
  p xx = Number . foldl1 f <$> mapM lowerInt xx

liftBoolVargOp :: ([Bool] -> Bool) -> Val
liftBoolVargOp f = PrimFunc $ return . Boolean . f . map lowerBool

-- TODO
liftIntBinOp :: (Int -> Int -> Int) -> Val
liftIntBinOp f = PrimFunc p where 
  p [x,y] = do xx <- lowerInt x
               yy <- lowerInt y
               return (Number (f xx yy))
                -- return Number (f xx <$> lowerInt y)
  p v = throwError $ UnexpectedArgs v

 -- f c = PrimFunc p where
--   p [] = return $ Number c
--   p [x] = Number . f c <$> lowerInt x
--   p xx = Number . foldl1 f <$> mapM lowerInt xx

  -- You should replace the following line with your own implementation
  -- PrimFunc . const $ unimplemented "Lifting binary integer operator (`liftIntBinOp`)"

-- TODO
liftIntUnaryOp :: (Int -> Int) -> Val
liftIntUnaryOp f = PrimFunc p where
	p [x] = Number . f <$> (lowerInt x)
	p v = throwError $ UnexpectedArgs v

	-- p xx = Number . f <$> mapM lowerInt xx

  -- You should replace the following line with your own implementation
  -- PrimFunc . const $ unimplemented "Lifting unary integer operator (`liftIntUnaryOp`)"

liftBoolUnaryOp :: (Bool -> Bool) -> Val
liftBoolUnaryOp f = PrimFunc p where
  p [Boolean False] = return $ Boolean True
  p [Boolean True] = return $ Boolean False
  p [x] = return $ Boolean False
  p v = throwError $ UnexpectedArgs v

-- TODO
liftCompOp :: (Int -> Int -> Bool) -> Val
liftCompOp f = PrimFunc p where
	p [] = return $ Boolean True
	p [x] = return $ Boolean True
	-- Number . foldl1 f <$> mapM lowerInt xx
	p xx = Boolean . eval f <$> mapM lowerInt xx where
		   eval f [x,y] = f x y
		   eval f (x:y:ys) = (f x y) && (eval f (y:ys))  
	-- p [a, b] = return $ Boolean $ f True
	-- p [_] = return $ Boolean $
  -- You should replace the following line with your own implementation
  -- PrimFunc . const $ unimplemented "Lifting comparison operator (`liftCompOp`)"

--- ### Primtive operations
car :: [Val] -> EvalState Val
car [List (x:xx)] = return x
car [DottedList (x:xx) xs] = return x
car [xs] = return xs
car v = throwError $ UnexpectedArgs v

-- car (x:xx) = return x


-- Primitive function `cdr`
-- TODO
cdr :: [Val] -> EvalState Val
-- cdr (x:xx) = return xx
cdr [List (x:xx)] = return (List xx)
cdr [DottedList (x:xx) xs] = return (DottedList xx xs)
-- cdr [_] = return (List [])
cdr v = throwError $ UnexpectedArgs v

-- cdr (x:xx) = return (List xx)

-- Primitive function `cons`
-- -- TODO
-- cons :: [Val] -> EvalState Val
-- cons (x:xx) = return (DottedList (cons xx) x)
cons :: [Val] -> EvalState Val
cons [x,y] = return (flattenList (DottedList [x] y))
cons xs = throwError (UnexpectedArgs xs)


-- Primitive function `append`
append :: [Val] -> EvalState Val
append [] = return $ List []
append [x] = return x
append vv = foldlM append' (List []) (map flattenList vv) where
  append' (List []) x = return x
  append' (List xs) (List ys) = return $ List (xs ++ ys)
  append' (List xs) (DottedList ys y) = return $ DottedList (xs ++ ys) y
  append' _ acc = throwError $ TypeError acc

-- Primitive function `apply`
-- It applies a function to a list of parameters
-- TODO
-- Examples:
--   (apply + '(1 2 3))  => 6
--   (apply car '((1 2 3)))  => 1
applyPrim :: [Val] -> EvalState Val
applyPrim [(PrimFunc f), (List args)] = f args

-- Primitive function `eval`
-- It evaluates the single argument as an expression
-- All you have to do is to check the number of arguments and
-- feed the single argument to the evaluator!
-- TODO
-- Examples:
--   (eval '(+ 1 2 3))  => 6

list :: [Val] -> EvalState Val
list [] = return (List [])
list xx = return (flattenList (List xx))


evalPrim :: [Val] -> EvalState Val
evalPrim [x] = eval x 
evalPrim xx = throwError $ UnexpectedArgs xx

-- Primitive function `=`, throwing type error for mismatch
-- `=` is a comparison operator for numbers and booleans
-- TODO
-- Examples:
--   (= 1 1) => #t
--   (= #f #t) => #f
--   (= #f #f) => #t
--   (= 'a 10) => Type error
--   (= 'a 'b) => Type error
equalSign :: [Val] -> EvalState Val
-- equalSign = const $ unimplemented "Primitive function `=`"
equalSign [] = return $ Boolean True
equalSign [x] = return $ Boolean True
equalSign (x1:x2:xs) = if comp x1 x2 then (equalSign (x2:xs)) else return (Boolean False)
      where comp (Number a) (Number b) = (a == b)
            comp (Boolean a) (Boolean b) = (a == b)
            -- comp (Number _) b = throwError $ TypeError b
            -- comp (Boolean _) b = throwError $ TypeError b
            -- comp a b = throwError $ TypeError b
-- equalSign [] = return $ Boolean True
-- equalSign [(Number i), (Number j)] = return $ Boolean (i == j)
-- equalSign [(Boolean i), (Boolean j)] = return $ Boolean (i == j)
-- equalSign [x] = throwError $ TypeError x
-- equalSign , _] = throwError $ TypeError []
-- equalSign x1:x2:xx = equalSign x1:x2
-- Primitive function `car`
-- -- TODO---------------------------------------------------------------------
-- car :: [Val] -> EvalState Val
-- car = const $ unimplemented "Primitive function `car`"

-- -- Primitive function `cdr`
-- -- TODO
-- cdr :: [Val] -> EvalState Val
-- cdr = const $ unimplemented "Primitive function `cdr`"

-- -- Primitive function `cons`
-- -- TODO
-- cons :: [Val] -> EvalState Val
-- cons = const $ unimplemented "Primitive function `cons`"

-- -- Primitive function `append`
-- append :: [Val] -> EvalState Val
-- append [] = return $ List []
-- append [x] = return x
-- append vv = foldlM append' (List []) (map flattenList vv) where
--   append' (List []) x = return x
--   append' (List xs) (List ys) = return $ List (xs ++ ys)
--   append' (List xs) (DottedList ys y) = return $ DottedList (xs ++ ys) y
--   append' _ acc = throwError $ TypeError acc

-- -- Primitive function `apply`
-- -- It applies a function to a list of parameters
-- -- TODO
-- -- Examples:
-- --   (apply + '(1 2 3))  => 6
-- --   (apply car '((1 2 3)))  => 1
-- applyPrim :: [Val] -> EvalState Val
-- applyPrim = const $ unimplemented "Primitive function `apply`"

-- -- Primitive function `eval`
-- -- It evaluates the single argument as an expression
-- -- All you have to do is to check the number of arguments and
-- -- feed the single argument to the evaluator!
-- -- TODO
-- -- Examples:
-- --   (eval '(+ 1 2 3))  => 6
-- evalPrim :: [Val] -> EvalState Val
-- evalPrim = const $ unimplemented "Primitive function `eval`"

-- -- Primitive function `=`, throwing type error for mismatch
-- -- `=` is a comparison operator for numbers and booleans
-- -- TODO
-- -- Examples:
-- --   (= 1 1) => #t
-- --   (= #f #t) => #f
-- --   (= #f #f) => #t
-- --   (= 'a 10) => Type error
-- --   (= 'a 'b) => Type error
-- equalSign :: [Val] -> EvalState Val
-- equalSign = const $ unimplemented "Primitive function `=`"
-------------------------------------------------------------------------
-- Primitive function `eq?`, not throwing any error
-- `eq?` is a comparison operator for atom values (numbers, booleans, and symbols)
-- Returns `#f` on type mismatch or unsupported types (functions etc)
-- TODO
-- Examples:
--   (eq? 1 1) => #t
--   (eq? #f #t) => #f
--   (eq? #f #f) => #t
--   (eq? 'a 10) => #f
--   (eq? 'a 'a) => #t
eq :: [Val] -> EvalState Val
-- eq = const $ unimplemented "Primitive function `eq?`"
eq [] = return $ Boolean True
eq [x] = return $ Boolean True
eq (x1:x2:xs) = if comp x1 x2 then (eq (x2:xs)) else return (Boolean False)
      where comp (Number a) (Number b) = (a == b)
            comp (Symbol a) (Symbol b) = (a == b)
            comp (Boolean a) (Boolean b) = (a == b)

-- Primitive function `list?` predicate
-- `(list? arg)` determines whether `arg` is a non-dotted list
-- or an empty list (null)
-- TODO
isList :: [Val] -> EvalState Val
-- isList [] = return null
-- isList x:xs =  
	-- isList :: [Val] -> EvalState Val
isList [v] = return . Boolean $ case flattenList v of
                                     List _ -> True
                                     _ -> False
isList vv = throwError $ UnexpectedArgs vv
	-- const $ unimplemented "Primitive function `list?`"

-- Primitive function `symbol?` predicate
-- TODO
isSymbol :: [Val] -> EvalState Val
-- isSymbol :: [Val] -> EvalState Val
isSymbol [Symbol _] = return $ Boolean True
isSymbol [_] = return $ Boolean False
isSymbol vv = throwError $ UnexpectedArgs vv 
	-- const $ unimplemented "Primitive function `symbol?`"

-- Primitive function `pair?` predicate
-- Any `List` or `DottedList` is a pair
-- TODO
-- isPair :: [Val] -> EvalState Val
isPair :: [Val] -> EvalState Val
isPair [List []] = return $ Boolean False 
isPair [List _] = return $ Boolean True
isPair [DottedList _ _ ] = return $ Boolean True
isPair [_] = return $ Boolean False
isPair vv = throwError $ UnexpectedArgs vv 
 
	-- const $ unimplemented "Primitive function `pair?`"

-- Primitive function `number?` predicate
-- TODO
isNumber :: [Val] -> EvalState Val
isNumber [Number _] = return $ Boolean True
isNumber [_] = return $ Boolean False
isNumber vv = throwError $ UnexpectedArgs vv
-- isNumber = const $ unimplemented "Primitive function `number?`"

-- Primitive function `boolean?` predicate
-- TODO
isBoolean :: [Val] -> EvalState Val
isBoolean [Boolean _] = return $ Boolean True
isBoolean [_] = return $ Boolean False
isBoolean vv = throwError $ UnexpectedArgs vv
-- isBoolean = const $ unimplemented "Primitive function `boolean?`"

-- Primitive function `null?` predicate
-- An empty list or its *equivalent* value is null
-- Note: Think about what's equivalent
-- TODO
isNull:: [Val] -> EvalState Val
-- isNull [x] = case x of List [] -> return (Boolean True)
--                        _ -> return (Boolean False)

-- isNull xs = throwError $ UnexpectedArgs xs 

-- isNull :: [Val] -> EvalState Val
isNull [List []] = return $ Boolean True
isNull [List _] = return $ Boolean False
isNull [_] = return $ Boolean False
isNull vv = throwError $ UnexpectedArgs vv
-- -- isNull = const $ unimplemented "Primitive function `null?`"

--- ### Runtime
runtime :: Env
runtime = H.fromList [ ("+", liftIntVargOp (+) 0)
                     , ("-", liftIntVargOp (-) 0)
                     , ("*", liftIntVargOp (*)  1)
                     , ("/", liftIntVargOp (div) 1)
                     , ("and", liftBoolVargOp and)
                     , ("or", liftBoolVargOp or)
                     , ("<", liftCompOp (<))
                     , (">", liftCompOp (>))
                     , (">=", liftCompOp (>=))
                     , ("<=", liftCompOp (<=))
                     , ("car", PrimFunc car)
                     , ("cdr", PrimFunc cdr)
                     , ("cons", PrimFunc cons)
                     , ("list", PrimFunc list)
                     , ("not", liftBoolUnaryOp not)
                     , ("=", PrimFunc equalSign)
                     , ("eq?",PrimFunc eq)
                     , ("modulo", liftIntBinOp mod)
                     , ("abs", liftIntUnaryOp abs)
                     , ("append", PrimFunc append)
                     , ("symbol?", PrimFunc isSymbol)
                     , ("list?", PrimFunc isList)
                     , ("pair?", PrimFunc isPair)
                     , ("number?", PrimFunc isNumber)
                     , ("boolean?", PrimFunc isBoolean)
                     , ("null?", PrimFunc isNull)
                     , ("apply", PrimFunc applyPrim)
                     , ("eval", PrimFunc evalPrim)]