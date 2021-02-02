module Lib
    ( checkArgs,
      transformToAst,
      displayAst,
      transformAst,
      findMissingInExpr,
      getMissingExpr,
      replaceInExpr,
      printDisplay,
      checkAllLists,
      findNumberOfOccurences
    ) where

import Exec
import Control.Exception
import Data.Maybe
import System.IO.Error
import Exec
import Text.Read
import System.Exit


handleError :: IOError -> Maybe String
handleError a
    | isDoesNotExistError a = Just "File does not exist"
    | isPermissionError a = Just "User doesn't have permission"
    | otherwise = Nothing

doesFileExist :: FilePath -> IO (Bool, String)
doesFileExist filePath = do
                        isGood <- tryJust handleError (readFile filePath)
                        case isGood of
                                Left error -> return (False, error)
                                Right file -> return (True, file)

checkArgs :: [String] -> IO [String]
checkArgs [] = return []
checkArgs args = do
                tmp <- doesFileExist (head args)
                checked <- checkArgs (tail args)
                case tmp of
                        (False, error) -> do
                                        putStr (head args)
                                        putStr ": "
                                        putStrLn error
                                        return []
                        (True, file) -> return ([file] ++ checked)

transformToAst :: [String] -> [[Expr]]
transformToAst [] = []
transformToAst (x:xs) = [myParser x]++(transformToAst xs)

getMissingExpr :: [Expr] -> Expr -> [Expr]
getMissingExpr [] a = []
getMissingExpr (x:xs) a = case x == a of
  True -> xs
  False -> getMissingExpr xs a

checkAllLists :: [[Expr]] -> Expr -> Int -> [Expr]
checkAllLists [] toFind n = []
checkAllLists xp toFind 0 = getMissingExpr (head xp) toFind
checkAllLists xp toFind n = case (getMissingExpr (xp !! n) toFind) == [] of
  True -> checkAllLists xp toFind (n - 1)
  False -> getMissingExpr (xp !! n) toFind 

findMissingInExpr :: [Expr] -> Int -> Int -> Expr
findMissingInExpr [] n current = Null
findMissingInExpr (x:xs) n current
  | x == (Exec.Symbol "eq?") = findMissingInExpr xs n current
  | x == (Exec.Symbol "cons") = findMissingInExpr xs n current
  | x == (Exec.Symbol "car") = findMissingInExpr xs n current
  | x == (Exec.Symbol "cdr") = findMissingInExpr xs n current
  | x == (Exec.Symbol "atom?") = findMissingInExpr xs n current
  | x == (Exec.Symbol "define") = findMissingInExpr xs n current
  | otherwise = case x of
    (Exec.Symbol a) -> case (n - 1) == current of
      True -> x
      False -> case (findNumberOfOccurences (x:xs)) > 1 of
        True -> findMissingInExpr xs n current
        False -> x
    _ -> findMissingInExpr xs n current

findNumberOfOccurences :: [Expr] -> Int
findNumberOfOccurences [] = 0
findNumberOfOccurences (x:xs) = case x of
  (Exec.Symbol a) -> 1 + (findNumberOfOccurences xs)
  _ -> findNumberOfOccurences xs

replaceInExpr :: [Expr] -> Expr -> [Expr] -> [Expr]
replaceInExpr [] toChange xp = []
replaceInExpr (x:xs) toChange xp = case x == toChange && xp /= [] of
  True -> (init xp)++xs
  False -> [x]++(replaceInExpr xs toChange xp)

replaceNthElement :: [[Expr]] -> Int -> [Expr] -> [[Expr]]
replaceNthElement (x:xs) 0 xp = [xp]++xs
replaceNthElement (x:xs) n xp = [x]++(replaceNthElement xs (n - 1) xp)

transformAst :: [[Expr]] -> [[Expr]] -> Int -> [Ast]
transformAst [] xp n = []
transformAst (x:[]) xp 0 = [checkInAst x]
transformAst (x:xs) xp n
  | n == 0 = [checkInAst x]++(transformAst xs xp (n + 1))
  | otherwise = case findMissingInExpr x (length xp) n of
    Null -> [checkInAst x]++(transformAst xs xp (n + 1))
    Exec.Symbol a -> [checkInAst secondReplaced]++(transformAst xs (replaceNthElement xp n xReplaced) (n + 1))
    where
      a = findMissingInExpr x (length xp) n
      replaced = (replaceInExpr x a (checkAllLists xp a (n - 1)))
      b = (findMissingInExpr replaced (length xp) n)
      secondReplaced = (replaceInExpr replaced b (checkAllLists xp b (n - 1)))
      xReplaced = (replaceInExpr (xp !! n) a (checkAllLists xp a (n - 1)))

exprToString :: Expr -> String
exprToString (Exec.Number a) = show a
exprToString (MyBool a) = case a of
  True -> "#t"
  False -> "#f"
exprToString (Exec.Symbol a) = a
exprToString (Operator Nil) = "()"
exprToString (Quote z) = z
exprToString a = "Null"

divString :: String -> String -> String
divString a b = case readMaybe a :: Maybe Int of
  Just (nb) -> case readMaybe b :: Maybe Int of
    Just (0) -> "Division with 0 is forbidden"
    Just (nb2) -> show (nb `div` nb2)
    Nothing -> "Error: it must be a number"
  Nothing -> "Error: it must be a number"

modString :: String -> String -> String
modString a b = case readMaybe a :: Maybe Int of
  Just (nb) -> case readMaybe b :: Maybe Int of
    Just (0) -> "modulo with 0 is forbidden"
    Just (nb2) -> show (nb `mod` nb2)
    Nothing -> "Error: it must be a number"
  Nothing -> "Error: it must be a number"

lowString :: String -> String -> String
lowString a b = case readMaybe a :: Maybe Int of
  Just (nb) -> case readMaybe b :: Maybe Int of
    Just (nb2) -> case (nb < nb2) of
      True -> "#t"
      False -> "#f"
    Nothing -> "Error: it must be a number"
  Nothing -> "Error: it must be a number"

equalCheck :: String -> String -> String
equalCheck a b
  | a == b = "#t"
  | otherwise = "#f"

handleOperatorList :: [Ast] -> Char -> String
handleOperatorList [] c = ""
handleOperatorList (fst : rst) c = case displayAst fst 0 of
  "error" -> "error bad syntax"
  nb -> case readMaybe nb :: Maybe Int of
    Nothing -> "error while reading int"
    Just (nb2) -> case handleOperatorList rst c of
      "" -> show (nb2)
      nb3 -> case readMaybe nb3 :: Maybe Int of
        Nothing -> "error while reading second int"
        Just (nb4) -> case c of
          '+' -> show (nb2 + nb4)
          '-' -> show (nb2 - nb4)
          '*' -> show (nb2 * nb4)

displayAst :: Ast -> Int -> String
displayAst (EndResult a) n = exprToString a
displayAst (MyCons a b) n = case n of
  0 -> case a of
    (EndResult c) -> case b of
      (EndResult d) -> case d of
        (Operator Nil) -> "(" ++ (exprToString c) ++ ")"
        _ -> "(" ++ (exprToString c) ++ " . " ++ (exprToString d) ++ ")"
      _ -> "(" ++ (exprToString c) ++ " " ++ (displayAst b 1) ++ ")"
    _ -> "((" ++ (displayAst a 1) ++ ") " ++ (displayAst b 1) ++ ")"
  _ -> case a of
    (EndResult c) -> case b of
      (EndResult d) -> case d of
        (Operator Nil) -> (exprToString c)
        _ -> (exprToString c) ++ " . " ++ (exprToString d)
      _ -> (exprToString c) ++ " " ++ (displayAst b n)
    _ -> "(" ++ (displayAst a n) ++ ") " ++ (displayAst b n)
displayAst (MyCar b) n = displayAst b n
displayAst (MyCdr b) n = displayAst b n
displayAst (MyAtom b) n = displayAst b n
displayAst (MyDiv a b) n = divString (displayAst a n) (displayAst b n)
displayAst (MyMod a b) n = modString (displayAst a n) (displayAst b n)
displayAst (MyLow a b) n = lowString (displayAst a n) (displayAst b n)
displayAst (MyAdd list) n = handleOperatorList list '+'
displayAst (MySub list) n = handleOperatorList list '-'
displayAst (MyMul list) n = handleOperatorList list '*'
displayAst (MyEq a b) n = equalCheck (displayAst a n) (displayAst b n)
displayAst (Define a b) n = displayAst b n
displayAst (Error) n = "error"

printDisplay :: String -> IO ()
printDisplay "Null" = do
  putStrLn "There is an error"
  exitWith (ExitFailure 84)
printDisplay "error while reading int" = do
  putStrLn "error while reading int"
  exitWith (ExitFailure 84)
printDisplay "error while reading second int" = do
  putStrLn "error while reading second int"
  exitWith (ExitFailure 84)
printDisplay "Division with 0 is forbidden" = do
  putStrLn "Division with 0 is forbidden"
  exitWith (ExitFailure 84)
printDisplay "Error: it must be a number" = do
  putStrLn "Error: it must be a number"
  exitWith (ExitFailure 84)
printDisplay "modulo with 0 is forbidden" = do
  putStrLn "modulo with 0 is forbidden"
  exitWith (ExitFailure 84)
printDisplay "error bad syntax" = do
  putStrLn "error bad syntax"
  exitWith (ExitFailure 84)
printDisplay "error" = do
  putStrLn "error"
  exitWith (ExitFailure 84)
printDisplay a = do
  putStrLn a
  exitWith (ExitSuccess)