--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk x k
    | x <= 1 = k 1 
    | otherwise = factk (x - 1) (\v -> k (x * v))
    
--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] k1 k2 | even x    = k1 x
                  | otherwise = k2 x
evenoddk (x:xs) k1 k2 | even x    = evenoddk xs (\v -> k1 $ v + x) k2
                     | otherwise = evenoddk xs k1 (\v -> k2 $ v + x)
-- evenoddk xx@(x:xs) k1 k2
--     | (last xx) `mod` 2 == 0 

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (VarExp s) = True
isSimple (IntExp e) = True
isSimple (IfExp e1 e2 e3) = all isSimple [e1,e2,e3]
isSimple (OpExp op e1 e2) = isSimple e1 && isSimple e2
isSimple (AppExp _ _) = False


--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k syms = (AppExp k (IntExp i), syms)
cpsExp (VarExp i) k syms = (AppExp k (VarExp i), syms)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k n 
      | isSimple e =  (AppExp (AppExp f e) k, n)
      | otherwise = 
      	let (v,b) = gensym n 
      	in cpsExp e (LamExp v (AppExp (AppExp f (VarExp v)) k)) b

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k n
      | (isSimple e1) && (isSimple e2) = (AppExp k (OpExp op e1 e2), n)
      | (isSimple e1) = let (v,b) = gensym n in cpsExp e2 (LamExp v (AppExp k (OpExp op e1 (VarExp v)))) b
      | (isSimple e2) = let (v,b) = gensym n in cpsExp e1 (LamExp v (AppExp k (OpExp op (VarExp v) e2))) b
      | otherwise = cpsExp e1 (LamExp v1 e) b3
                  where (v1, b1) = gensym n
                        (v2, b2) = gensym b1
                        (e,b3) = cpsExp e2 (LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))) b2
                      
--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k n  
      | isSimple e1 = let (k1, b1) = cpsExp e2 k n
                          (k2, b2) = cpsExp e3 k n 
                      in ((IfExp e1 k1 k2), n)
      | otherwise = let (v,b) = gensym n 
                        (t1, b1) = cpsExp e2 k b
                        (t2, b2) = cpsExp e3 k b
                    in cpsExp e1 (LamExp v (IfExp (VarExp v) t1 t2)) b
--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl s1 s2 e) = Decl s1 (s2++["k"]) k1 
                          where (k1,b1) = cpsExp e (VarExp "k") 1
