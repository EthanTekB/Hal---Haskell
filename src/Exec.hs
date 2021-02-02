module Exec where

import Data.Char
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Text.Read (readMaybe)
import Prelude

data ParseOprt
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Lower
  | Greater
  | OpenPar
  | ClosePar
  | Nil
  | ParseErr
  deriving (Eq, Show)

data Expr
  = Operator ParseOprt
  | Quote String
  | Number Int
  | MyBool Bool
  | Symbol String
  | Cons [Expr]
  | Null
  deriving (Eq, Show)

data Ast
  = MyCons Ast Ast
  | MyCar Ast
  | MyCdr Ast
  | MyAtom Ast
  | MyDiv Ast Ast
  | MyMod Ast Ast
  | MyLow Ast Ast
  | MyAdd [Ast]
  | MySub [Ast]
  | MyMul [Ast]
  | MyEq Ast Ast
  | Define Ast Ast
  | EndResult Expr
  | Error
  deriving (Eq, Show)

checkInAst :: [Expr] -> Ast
checkInAst [] = Error
checkInAst (fst : rst)
  | fst == (Operator Add) = MyAdd (addHandle rst)
  | fst == (Operator Sub) = MySub (addHandle rst)
  | fst == (Operator Mul) = MyMul (addHandle rst)
  | fst == (Symbol "eq?") = case argExpr rst (-1) of
    ([Null], [Null]) -> Error
    (a, b) -> case argExpr b (-1) of
      ([Null], [Null]) -> Error
      (x, y) -> ((MyEq (checkInAst a)) (checkInAst x)) --(MyDiv (EndResult (Number (divExpr (head a) (head x)))))
  | fst == (Operator Div) = case argExpr rst (-1) of
    ([Null], [Null]) -> Error
    (a, b) -> case argExpr b (-1) of
      ([Null], [Null]) -> Error
      (x, y) -> ((MyDiv (checkInAst a)) (checkInAst x)) --(MyDiv (EndResult (Number (divExpr (head a) (head x)))))
  | fst == (Operator Mod) = case argExpr rst (-1) of
    ([Null], [Null]) -> Error
    (a, b) -> case argExpr b (-1) of
      ([Null], [Null]) -> Error
      (x, y) -> ((MyMod (checkInAst a)) (checkInAst x))
  | fst == (Operator Lower) = case argExpr rst (-1) of
    ([Null], [Null]) -> Error
    (a, b) -> case argExpr b (-1) of
      ([Null], [Null]) -> Error
      (x, y) -> ((MyLow (checkInAst a)) (checkInAst x))
  | fst == (Symbol "cons") = case argExpr rst (-1) of
    ([Null], [Null]) -> Error
    (a, b) -> case argExpr b (-1) of
      ([Null], [Null]) -> Error
      (x, _) -> (MyCons (checkInAst a) (checkInAst x))
  | fst == (Symbol "car") = case argExpr rst (-1) of
    ([Null], [Null]) -> Error
    (a, b) -> case checkInAst a of
      (MyCons x y) -> (MyCar x)
      (EndResult (Quote str)) -> case str of
        ('(' : rest) -> checkInAst (myParser ("car " ++ ("(cons " ++ rest)))
        _ -> (MyCar (EndResult (Quote str)))
      _ -> Error
  | fst == (Symbol "cdr") = case argExpr rst (-1) of
    ([Null], [Null]) -> Error
    (a, b) -> case checkInAst a of
      (MyCons x y) -> (MyCdr y)
      (EndResult (Quote str)) -> case str of
        ('(' : rest) -> checkInAst (myParser ("cdr " ++ ("(cons " ++ rest)))
        _ -> (MyCdr (EndResult (Quote str)))
      _ -> Error
  | fst == (Symbol "atom?") = case argExpr rst (-1) of
    ([Null], [Null]) -> Error
    (a, b) -> case checkInAst a of
      (MyCons x y) -> (MyAtom (EndResult (MyBool False)))
      EndResult (Operator Nil) -> (MyAtom (EndResult (MyBool True)))
      EndResult (Quote str) -> case str of
        ('(' : rest) -> (MyAtom (EndResult (MyBool False)))
        _ -> (MyAtom (EndResult (MyBool True)))
      _ -> (MyAtom (EndResult (MyBool True)))
  -- (EndResult (Operator Quote))  -> case checkInAst b of
  --      (EndResult (Symbol _)) -> (MyAtom (EndResult (MyBool True)))
  --    (EndResult (Operator (OpenPar))) -> (MyAtom (EndResult (MyBool False)))
  | fst == (Operator OpenPar) = checkInAst rst
  | fst == (Symbol "define") = case argExpr rst (-1) of
    ([Null], [Null]) -> Error
    (a, b) -> case checkInAst a of
      (EndResult (Symbol str)) -> case argExpr b (-1) of
        ([Null], [Null]) -> Error
        (c, d) -> case checkInAst c of
          (e) -> Define (EndResult (Symbol str)) e
          -- (EndResult (Number n)) -> Define (EndResult (Symbol str)) (EndResult (Number n))
          -- _ -> Error
      _ -> Error
  | otherwise = (EndResult fst)

addHandle :: [Expr] -> [Ast]
addHandle [] = []
addHandle (Operator ClosePar : _) = []
addHandle list = case argExpr list (-1) of
  ([Null], [Null]) -> []
  (a, b) -> (checkInAst a) : (addHandle b)

operatorCase :: Char -> String -> ParseOprt
operatorCase char str = case (char) of
  '+' -> Add
  '-' -> Sub
  '*' -> Mul
  '/' -> Div
  '%' -> Mod
  '^' -> Pow
  '<' -> Lower
  '>' -> Greater
  '(' -> OpenPar
  ')' -> ClosePar
  '\'' -> case checkQuote str of
    True -> Nil
  _ -> ParseErr

checkQuote :: String -> Bool
checkQuote ('(' : ')' : rst) = True
checkQuote a = False

type Parser a = String -> Maybe (a, String)

parseChar :: Char -> Parser Char
parseChar _ [] = Nothing
parseChar a (b : c)
  | a == b = Just (a, c)
  | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] _ = Nothing
parseAnyChar (a : b) (x : xs)
  | a == x = Just (x, xs)
  | otherwise = parseAnyChar b (x : xs)

parseOr :: Parser a -> Parser a -> Parser a
parseOr a b c = case a c of
  Just a -> Just a
  Nothing -> case b c of
    Just b -> Just b
    Nothing -> Nothing

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd a b other = case a other of
  Nothing -> Nothing
  Just a -> case b (snd a) of
    Nothing -> Nothing
    Just b -> Just ((fst a, fst b), snd b)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith a b c other = case b other of
  Nothing -> Nothing
  Just b -> case c (snd b) of
    Nothing -> Nothing
    Just c -> Just (a (fst b) (fst c), snd c)

parseMany :: Parser a -> Parser [a]
parseMany a other = case a other of
  Just (y, z) -> case parseMany a z of
    Nothing -> Just ([y], z)
    Just (v, w) -> Just (y : v, w)
  Nothing -> Just ([], other)

parseSome :: Parser a -> Parser [a]
parseSome a [] = Nothing
parseSome a str = case a str of
  Nothing -> Nothing
  Just (y, z) -> case parseSome a z of
    Nothing -> Just ([y], z)
    Just (v, w) -> Just (y : v, w)

parseInt :: Parser Int
parseInt str = case readMaybe str :: Maybe Int of
  Just x -> Just (x, "")
  Nothing -> Nothing

parseNbStr :: Parser String
parseNbStr str = case (parseOr (parseStrFloat) (parseStrInt) str) of
  Nothing -> Nothing
  Just (x, y) -> Just (x, y)

parseStrFloat :: Parser String
parseStrFloat [] = Nothing
parseStrFloat str = parseAndWith (\x (x', y') -> x ++ [x'] ++ y') (parseSome (parseAnyChar ['0' .. '9'])) (parseAnd (parseChar '.') (parseSome (parseAnyChar ['0' .. '9']))) str

parseStrInt :: Parser String
parseStrInt [] = Nothing
parseStrInt str = (parseSome (parseAnyChar ['0' .. '9'])) str

checkDivOrMod :: String -> ParseOprt
checkDivOrMod ('d' : 'i' : 'v' : rest) = Div
checkDivOrMod ('m' : 'o' : 'd' : rest) = Mod
checkDivOrMod str = ParseErr

handleQuote :: String -> Int -> (Expr, String)
handleQuote "" n = (Null, "")
handleQuote (x : xs) n
  | n == (-1) = (Quote "", xs)
  | x == '(' = case handleQuote xs (n + 1) of
    (Null, "") -> (Null, "")
    (Quote a, b) -> (Quote (x : a), b)
  | x == ')' && n == 1 = (Quote ")", xs)
  | x == ')' && n == 0 = (Quote "", (x : xs))
  | x == ' ' && n == 0 = (Quote "", xs)
  | otherwise = case handleQuote xs n of
    (Null, "") -> (Quote [x], "")
    (Quote a, b) -> (Quote (x : a), b)

checkIfQuote :: String -> Bool
checkIfQuote ('q':'u':'o':'t':'e':rst) = True
checkIfQuote whatEver = False

myParser :: String -> [Expr]
myParser [] = []
myParser (first : rst)
  | checkIfQuote (first:rst) == True = case handleQuote (tail (tail (tail (tail (tail rst))))) 0 of
      (Quote v, n) -> case checkQuote v of
        True -> Operator Nil : myParser n
        False -> Quote v : myParser n
      (Null, "") -> Null : myParser rst
  | first == '\'' = case handleQuote rst 0 of
      (Quote v, n) -> case checkQuote v of
        True -> Operator Nil : myParser n
        False -> Quote v : myParser n
      (Null, "") -> Null : myParser rst
  | checkDivOrMod (first : rst) /= ParseErr = Operator (checkDivOrMod (first : rst)) : myParser (tail (tail (tail rst)))
  | elem first "+-*/%^<>()" == True = Operator (operatorCase first rst) : myParser rst
  -- True -> Operator (operatorCase first rst) : myParser (tail (tail rst))
  -- False ->
  | elem first "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!?:;,." == True = case myWords (first : rst) of
    Just (a, ab) -> Symbol a : myParser ab
    Nothing -> error "ERROR: Invalid Parsing"
  | isDigit first == True = case parseNbStr (first : rst) of
    Just (a, b) -> case parseInt a of
      Just (c, cc) -> Number c : myParser b
      Nothing -> error "ERROR: 2nd case Invalid Parsing with Numbers"
    Nothing -> error "ERROR: 1st case Invalid Parsing with Numbers"
  | isSpace first == True = myParser rst
  where
    myWords = parseSome (parseAnyChar "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPRQTUVWXYZ!?:;,.")

--nullHandle :: String
--nullHandle [] = []
--nullHandle arg = parseAndWith (\ x (y,z) -> x++y:[z]) (parseSome (parseChar '\'')) (parseAnd (parseChar '(') (parseChar ')')) arg

cons :: a -> a -> (a, a)
cons arg1 arg2 = (arg1, arg2)

car :: [a] -> a
car (x : xs) = x

cdr :: [a] -> [a]
cdr (x : xs) = xs

data Type t
  = I Int
  | S String
  | V t
  | L [t]
  deriving (Show)

eqInt :: Int -> Int -> Bool
eqInt arg1 arg2
  | arg1 == arg2 = True
  | otherwise = False

eqSymb :: String -> String -> Bool
eqSymb arg1 arg2
  | arg1 == arg2 = True
  | otherwise = False

eqList :: [a] -> [a] -> Bool
eqList [] [] = True
eqList arg1 arg2 = False

atom :: [a] -> Bool
atom [] = True
atom arg1 = False

argExpr :: [Expr] -> Int -> ([Expr], [Expr])
argExpr [] 0 = ([], [])
argExpr (head : rst) parNum
  | parNum == 0 = ([], head : rst)
  | parNum == -1 && head == Operator OpenPar = case argExpr rst 1 of
    (x, y) -> (head : x, y)
  | head == Operator OpenPar = case argExpr rst (parNum + 1) of
    (x, y) -> (head : x, y)
  | head == Operator ClosePar = case argExpr rst (parNum - 1) of
    (x, y) -> (head : x, y)
  | parNum == -1 = ([head], rst)
  | head /= Operator OpenPar || head /= Operator ClosePar = case argExpr rst parNum of
    (x, y) -> (head : x, y)
  | otherwise = ([Null], [Null])
