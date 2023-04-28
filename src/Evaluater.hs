module Evaluater where

import StackMonad
import Lexer
import Text.Printf

falsey :: Sexpr -> Bool
falsey (ConstNumber 0) = True
falsey _ = False

type AssocList a = [(String,a)]

type WispVariable = (String,Sexpr)

lookupVar :: (Monad m, Eq k, PrintfArg k) => k -> StackT (k, v) m v
lookupVar k = do
    a <- lookupStack k
    case a of
        (Just v) -> return v
        Nothing  -> error $ printf "`%s' is unbound" k -- TODO: exceptions

evalnum :: (Monad m) => Sexpr -> StackT WispVariable m Double
evalnum e = do
    e' <- eval e
    case e' of
        (ConstNumber n) -> return n
        _ -> error "expected number"

evalBuiltinBinary :: (Monad m) =>
    (Double -> Double -> Double) -> Sexpr -> Sexpr -> StackT WispVariable m Sexpr

evalBuiltinBinary f a b = do
    a' <- evalnum a
    b' <- evalnum b
    return $ ConstNumber $ a' `f` b'

eval :: (Monad m) => Sexpr -> StackT WispVariable m Sexpr

{------ recurse down parens ------}
eval (Paren e) = eval e
eval (Paren e :-: ε) = do
    a <- eval e
    eval $ a :-: ε
eval (e :-: Paren ε) = do
    a <- eval ε
    eval $ e :-: a

{------ builtins ------}
eval (Define k v) = do
    v' <- eval v
    push (k,v')
    return v

eval (Lambda x body :-: arg) = do
    arg' <- eval arg
    push (x,arg')
    eval body

eval (Add a b) = evalBuiltinBinary (+) a b
eval (Subtract a b) = evalBuiltinBinary (-) a b
eval (Multiply a b) = evalBuiltinBinary (*) a b
eval (Divide a b) = evalBuiltinBinary (/) a b

{------ application ------}
eval (f :-: x) = do
    f' <- eval f
    case f' of
        (Lambda _ _) -> eval $ f' :-: x
        _            -> error "attempted to apply a non-abstraction"

{------ variables ------}
eval (Identifier w) = lookupVar w

{------ edge case ------}
eval x = return x

execProgram :: (Monad m) => [Sexpr] -> StackT WispVariable m ()
execProgram [] = return ()
execProgram (e:es) = do
    eval e
    execProgram es

