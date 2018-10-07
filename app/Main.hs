module Main where

import Lib
import Data.List as List
import Data.Maybe
import Data.List.Split
import Data.Char
import Data.Map as Map
import Data.Function
import Text.Printf

data Command =
  Assignment String Expr |
  Output String
    deriving (Show, Eq)

data Expr =
  Exp Operation Expr Expr |
  Value Int |
  Literal String |
  Reference String |
  NonValid String
    deriving (Show, Eq)

data Operation = Add | Mul
  deriving (Show, Eq)

isExp (Exp _ _ _) = True
isExp  _          = False

assignmentOpL = "<-"
assignmentOpR = "->"

isAssignmentL x = isInfixOf assignmentOpL x
isAssignmentR x = isInfixOf assignmentOpR x
isAssignment x = isAssignmentL x || isAssignmentR x
isOutput x = not $ isAssignment x
isWord s = List.all isAlpha s
isAdd s = isJust $ find (=='+') s
quote = '"'
quoted s = head s == quote && last s == quote
unquote = init . tail

parseExpr s
  | all isDigit s = Value $ read s
  | all isAlpha s = Reference s
  | quoted s = Literal $ unquote s
  | isAdd s =
      let elems = splitOn "+" s
          left  = parseExpr $ head elems
          right = parseExpr $ intercalate "+" $ tail elems
      in Exp Add left right
  | otherwise = NonValid s

parseAssignmentL s =
  let elems = splitOn assignmentOpL s
      name = elems !! 0
      value = parseExpr $ elems !! 1
  in Assignment name value

parseAssignmentR s =
  let elems = splitOn assignmentOpR s
      value = parseExpr $ elems !! 0
      name = elems !! 1
  in Assignment name value

parseCommand s
  | isAssignmentL s = parseAssignmentL s
  | isAssignmentR s = parseAssignmentR s
  | otherwise = Output s

toDict :: [Command] -> Map String Expr
toDict cmnds =
  toDict' cmnds Map.empty
    where
      toDict' []     hash = hash
      toDict' (x:xs) hash =
        case x of
          Assignment k v -> Map.insert k v dict
          Output     _   -> dict
        where dict = toDict' xs hash

evalExp (Exp Add a b) = (evalExp a) + (evalExp b)
evalExp (Exp Mul a b) = (evalExp a) * (evalExp b)
evalExp (Value x) = x


readVariables text =
    List.filter (/=' ') text
  & lines
  & List.map parseCommand
  & toDict

joinLines = intercalate "\n"

pairPrinter (k, v) = printf "Key %s maps to '%s'" (show k) (show v)

pairPrintEval (k, v) = printf "%s evaluated is %s" mapd evald
    where mapd = pairPrinter (k, v) :: String
          evald = show $ evalExp v

prettify f hash =
    toList hash
  & List.map f
  & joinLines

hashKeys   h = List.map fst $ toList h
hashValues h = List.map snd $ toList h

duplicate string n = concat $ replicate n string
lineFiller = duplicate "-" 30

main = do
  content <- readFile "lang/ex.txt"
  let variables = readVariables content
  -- putStrLn $ show variables
  --putStrLn "Full hashmap:"
  putStrLn lineFiller
  putStrLn $ prettify pairPrinter variables
  putStrLn lineFiller
  let expOnly = Map.filter isExp variables
  --putStrLn "Exp only:"
  --putStrLn $ prettify expOnly
  --putStrLn $ show $ hashValues expOnly
  putStrLn $ prettify pairPrintEval expOnly

  putStrLn "Which variable to read?"
  var <- getLine
  let found = Map.lookup var variables
  case found of
    Just v  -> putStrLn $ "found: " ++ (show v)
    Nothing -> putStrLn $ "not found"
