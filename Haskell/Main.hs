module Main where

import System.Environment
import Parser
import Evaluator

main :: IO ()
main = repl []


repl :: Env -> IO ()
repl e = getArgs >>= print. fst . eval e . readExpr . head


------------------------------ Testing from here on out -----------------------------------
testSimpleExpression s = fst $ eval [] (readExpr s)


simpleFunction s = a
    where (_ , e) = eval [] (readExpr "(function f (x) (* 3 x))")
          (a, _) = eval e (readExpr ("(f (" ++ s ++ "))"))

testFunctions e s = a
    where (_,e') = eval [] $ readExpr e
          (a,_) = eval e' $ readExpr s

factorials s = fst (eval (stringToEnv "(function factorial (x) (if (= x 1) 1 (* x (factorial (- x 1)))))") (readExpr ("(factorial ("++ s ++"))")))

stringToEnv :: String -> Env
stringToEnv s = snd (eval [] $ readExpr s)
