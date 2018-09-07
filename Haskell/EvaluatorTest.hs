module EvaluatorTest where

import Evaluator
import Parser

fun :: Env -> Expr -> (Expr, Env)
fun [] (LNum 1) = (eval [] (LNum 1)) -> ((LNum 1), [])





