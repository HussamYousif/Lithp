module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.Maybe


type Env =  [(String,  Expr)]

data Expr = LSymbol String
          | LNum Int
          | LBool Bool
          | LString String
          | List [Expr]
          | LAtom String
          | Nil
          | LFunction Expr Expr Env -- Expr (parameters) Expr (Body) Env (closure)
          deriving (Show, Eq)


showExpr :: String -> IO ()
showExpr = putStrLn

readExpr :: String -> Expr
readExpr input = case parse parseExpr "lisp" input of
    Left err -> LString $ "No match: " ++ show err
    Right val -> val

symbol :: Parser Char
symbol = oneOf "+-/*=<=>="

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser Expr
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "true" -> LBool True
                         "false" -> LBool False
                         _    -> LAtom atom

parseString :: Parser Expr
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"' --" -- The qoutations confuse vim
                return $ LString x

parseNumber :: Parser Expr
parseNumber =  (LNum . read) <$> many1 digit

parseList :: Parser Expr
parseList =  List <$> sepBy parseExpr spaces

parseExpr :: Parser Expr
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> do char '('
                x <- try parseList
                char ')'
                return x


newEnv :: Env
newEnv = []

extractAtom :: Expr -> String
extractAtom (LAtom a) = a

-- Sets parameters as nil
defineParameters :: Env -> [Expr]  -> Env
defineParameters e [] = e
defineParameters e (x:xs) = defineParameters (addVar n Nil e) xs
    where n = extractAtom x


-- Frst list are the param names, second list is the param values
bindParameters :: Env -> [Expr] -> [Expr] -> Env
bindParameters e [] _ = e
bindParameters e _ [] = e
bindParameters e [List (x:xs)] (y:ys) = bindParameters e' xs ys
    where e' = addVar (extractAtom x) y e
bindParameters e (x:xs) (y:ys) = bindParameters e' xs ys
    where e' = addVar (extractAtom x) y e

-- helper
find :: String -> Env -> Maybe Expr
find = lookup

getVar :: String -> Env -> Expr
getVar var env = case find var env of
                   Just a -> a
                   Nothing -> error ("Variable not defined: "  ++ var)

isBound :: String -> Env -> Bool
isBound var env = case find var env of
                    Just a -> True
                    Nothing -> False

addVar :: String -> Expr -> Env -> Env
addVar var ex env = case isBound var env of
                      True -> mutateVar var ex env
                      False -> (var, ex) : env


mutateVar :: String -> Expr -> Env -> Env
mutateVar v ex [] = []
mutateVar a ex (x:xs) = case a == fst x of
                               True -> (a,ex) : xs
                               False -> x : mutateVar a ex xs

displayVars :: Env -> IO ()
displayVars  = mapM_ print

