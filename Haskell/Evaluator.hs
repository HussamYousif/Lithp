module Evaluator where


import Parser


eval :: Env -> Expr -> (Expr, Env)
eval e (List [a]) = eval e a
eval e (List [] ) = (List [], e)
eval e (LNum a)  = (LNum a,e)
eval e s@(LString a) = (s, e)
eval e (LBool a) = (LBool a, e)
eval e Nil = (Nil, e)

    {-
eval e (List [Nil]) = (Nil,e)
eval e (List[LNum a])  = (LNum a, e)
eval e (List[LString a]) = (LString a, e)
eval e (List[LBool a]) = (LBool a, e)
eval e (List[LAtom a]) = (LAtom a, e)
-}

eval e (List [LAtom "function",LAtom n,param@(List p),b]) = (Nil, e')
    where e' = addVar n (LFunction param b (defineParameters [] p )) e

eval e (List [LAtom "quote", val]) = (val, e)
eval e (List [LAtom "head", List (x:_)]) = (x, e)
eval e (List [LAtom "tail", List (_:xs) ]) = (List xs, e)

eval e (List [LAtom "null", List l])
  | null l  =  (LBool True, e)
  | otherwise = (LBool False, e)

eval e (List [LAtom "atom", s]) = if isAtom s
                                   then (LBool True, e)
                                   else (LBool False, e)

eval e (List [LAtom "eq", LAtom a, LAtom b]) = (LBool (a==b), e)
eval e (List [LAtom "cons", a, List b]) = (List (a : b), e)

eval e (List [LAtom "mod", LNum a, LNum b]) = (LNum  (mod a b), e)
eval e (List [LAtom "bind", LAtom var, LNum a]) = (LNum a, Parser.addVar  var (LNum a) e)
eval e (List [LAtom "bind", LAtom var, LBool a]) = (LBool a, Parser.addVar  var (LBool a) e)
eval e (List [LAtom "bind", LAtom var, LString a]) = (LString a, Parser.addVar  var (LString a) e)
eval e (List [LAtom "bind", LAtom var, l@(List z)]) = (lx, addVar var lx le)
    where (lx, le) = eval e l


eval e (List [LAtom "+", LNum a, LNum b]) = (LNum (a+b), e)
eval e (List [LAtom "+", a, b]) = eval nne (List[LAtom "+", na, nb ])
    where (na, ne) = eval e a
          (nb, nne) = eval ne b

eval e (List [LAtom "++", List a, List b]) = (List (a ++ b), e)

eval e (List [LAtom "-", LNum a, LNum b]) = (LNum (a-b), e)
eval e (List [LAtom "-", a, b]) = eval nne (List[LAtom "-", na, nb ])
    where (na, ne) = eval e a
          (nb, nne) = eval ne b

eval e (List [LAtom "*", LNum a, LNum b]) = (LNum (a*b), e)
eval e (List [LAtom "*", a, b]) = eval nne (List[LAtom "*", na, nb ])
    where (na, ne) = eval e a
          (nb, nne) = eval ne b

eval e (List [LAtom "/", _ , LNum 0]) = error "Lithp Error: You tried to divide by zero"
eval e (List [LAtom "/", LNum a, LNum b]) = (LNum (a `div` b), e)
eval e (List [LAtom "/", a, b]) = eval nne (List[LAtom "/", na, nb ])
    where (na, ne) = eval e a
          (nb, nne) = eval ne b

eval e (List [LAtom "=", LNum a, LNum b]) = (LBool (a==b), e)
eval e (List [LAtom "=", a, b]) = eval nne (List[LAtom "=", na, nb ])
    where (na, ne) = eval e a
          (nb, nne) = eval ne b

eval e (List [LAtom "<=", LNum a, LNum b]) = (LBool (a<=b), e)
eval e (List [LAtom "<=", a, b]) = eval nne (List[LAtom "<=", na, nb ])
    where (na, ne) = eval e a
          (nb, nne) = eval ne b

eval e (List [LAtom ">=", LNum a, LNum b]) = (LBool (a>=b), e)
eval e (List [LAtom ">=", a, b]) = eval nne (List[LAtom ">=", na, nb ])
    where (na, ne) = eval e a
          (nb, nne) = eval ne b

eval e (List [LAtom "<", LNum a, LNum b]) = (LBool (a<b), e)
eval e (List [LAtom "<", a, b]) = eval nne (List[LAtom "<", na, nb ])
    where (na, ne) = eval e a
          (nb, nne) = eval ne b

eval e (List [LAtom ">", LNum a, LNum b]) = (LBool (a>b), e)
eval e (List [LAtom ">", a, b]) = eval nne (List[LAtom ">", na, nb ])
    where (na, ne) = eval e a
          (nb, nne) = eval ne b


eval e (List [LAtom "if", c, a, b])
                        | fst (eval e c) == LBool True = eval e a
                        | otherwise = eval e b

eval e fun@(List [LAtom a, List p]) = prepareFun e fun
eval e (List [LAtom "error", LString s ]) = error s
eval e (LAtom a) = (getVar a e, e)


eval e (List l) =  (List x, e')
    where (x, e') = mape'(mape eval e l)

isAtom :: Expr -> Bool
isAtom (LAtom _) = True
isAtom  (List [a]) = isAtom a
isAtom _ = False

mape' :: [(Expr, Env)] -> ([Expr], Env)
mape' l = ([x | (x, e) <- l], snd $ last l)

-- Takes function f,  e (environment) and a list [Expr] and applies f to every element while updating e at every step.
mape :: (Env -> Expr -> (Expr, Env)) -> Env -> [Expr] -> [(Expr,Env)]
mape _ e [] = []
mape f e (x:xs) = (y, e') : mape f e' xs
    where (y, e') = f e x

prepareFun :: Env -> Expr -> (Expr, Env)
prepareFun e (List [LAtom f, List p]) =  eval (addVar f fun e')  b
    where fun = getVar f e
          p' = funParam fun
          (p'', _)= eval e (List p)
          e' = bindParameters e p' (unpackList p'')
          b = funBody fun

unpackList :: Expr -> [Expr]
unpackList (List l) = l
unpackList a = [a]

              {-
prepareFun :: Env -> Expr -> (Expr, Env)
prepareFun e fu@(List [LAtom f, List p]) =eval (e'' ) b {-if (isBound f e'')
                                              then eval e'' b
                                              else eval e fu-}
     where fun = getVar f e
           p' = funParam fun
           e' = funEnv fun
           e'' = bindParameters e' p' p
           b = funBody fun
-}
-- Takes a function and changes it's environment
newFun :: Env -> Expr -> Expr
newFun e (LFunction p b _) = LFunction p b e

-- Extracts the environment of a function
funEnv :: Expr -> Env
funEnv (LFunction _ _ e) = e

funBody :: Expr -> Expr
funBody (LFunction _ b _ ) = b

funParam :: Expr -> [Expr]
funParam (LFunction (List p) _ _) = p
funParam (LFunction p _ _) = [p]

extractString :: Expr -> String
extractString (LAtom a) = a
extractString (LString a) = a

extractNum :: Expr -> Int
extractNum (LNum a) = a

extractBool :: Expr -> Bool
extractBool (LBool a) = a
