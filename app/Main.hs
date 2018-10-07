module Main where

import Lib
import Data.List as List
import Data.Maybe
import Data.List.Split
import Data.Char
import Data.Map as Map
import Data.Function
import Text.Printf

data Command = Assign String Expr
             | Output String
                deriving (Show, Eq)

data Expr = Exp Operation Expr Expr
          | Value Int
          | Literal String
          | Reference String
          | NonValid String
              deriving (Show, Eq)

data Operation = Add
               | Mul
                  deriving (Show, Eq)

isExp (Exp _ _ _) = True
isExp  _          = False

assignOpL = "<-"
assignOpR = "->"

isAssignL = isInfixOf assignOpL
isAssignR = isInfixOf assignOpR
isAssign x = isAssignL x || isAssignR x
isOutput = not . isAssign
isWord s = List.all isAlpha s
isAdd = isJust . (find (=='+'))
quote = '"'
quoted s = head s == quote && last s == quote
unquote = init . tail

parseExpr s
  | all isDigit s = Value $ read s
  | all isAlpha s = Reference s
  | quoted s = Literal $ unquote s
  | isAdd s = Exp Add left right
  | otherwise = NonValid s
  where elems = splitOn "+" s
        left  = parseExpr $ head elems
        right = parseExpr $ intercalate "+" $ tail elems


parseAssignL s = Assign name value
                      where elems = splitOn assignOpL s
                            name = elems !! 0
                            value = parseExpr $ elems !! 1

parseAssignR s = Assign name value
                      where elems = splitOn assignOpR s
                            value = parseExpr $ elems !! 0
                            name = elems !! 1

parseCommand s
  | isAssignL s = parseAssignL s
  | isAssignR s = parseAssignR s
  | otherwise = Output s


skiper x d = case x of Assign k v -> Map.insert k v d
                       Output  _   -> d

toDict :: [Command] -> Map String Expr
toDict cmnds = toDict' cmnds Map.empty
  where toDict' []     hash = hash
        toDict' (x:xs) hash = let dict = toDict' xs hash
                              in skiper x dict

evalExp :: Map String Expr -> Expr -> Int
evalExp d (Exp Add a b) = (evalExp d a) + (evalExp d b)
evalExp d (Exp Mul a b) = (evalExp d a) * (evalExp d b)
evalExp d (Reference y) = Map.lookup y d & fromJust & evalExp d
evalExp d (Value x)     = x
evalExp d (NonValid x)  = 0
evalExp d (Literal x)   = 0

duplicate string n = concat $ replicate n string
lineFiller = duplicate "-" 30


readVariables :: String -> Map String Expr
readVariables text = List.filter (/=' ') text
                   & lines
                   & List.map parseCommand
                   & toDict

joinLines = intercalate "\n"

pairPrinter (k, v) = printf "Key %s maps to '%s'" (show k) (show v)
pairPrintEval d (k, v) = printf "%s evaluated to %s" mapd evald
    where mapd = pairPrinter (k, v) :: String
          evald = show $ evalExp d v

prettify hash = toList hash
              & List.map (pairPrintEval hash)
              & joinLines

hashKeys   h = List.map fst $ toList h
hashValues h = List.map snd $ toList h


main = do
  content <- readFile "lang/ex.txt"
  let vars = readVariables content
  putStrLn lineFiller
  putStrLn $ prettify vars
  putStrLn lineFiller
  --let expOnly = Map.filter isExp variables
  --putStrLn "Exp only:"
  --putStrLn $ prettify expOnly
  --putStrLn $ show $ hashValues expOnly
  --putStrLn $ prettify (pairPrintEval varlist) variables

  --putStrLn "Which variable to read?"
  -- var <- getLine
  -- let found = Map.lookup var variables
  -- case found of
  --   Just v  -> putStrLn $ "found: " ++ (show v)
  --   Nothing -> putStrLn $ "not found"
