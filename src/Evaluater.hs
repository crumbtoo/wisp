{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Evaluater where

import Stack as S
import Lexer
import Text.Printf
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Map.Strict as M
import Debug.Trace
import WispMonad

getVar :: (WispMonad m) => String -> m Sexpr
getVar k = do
    a <- lookupStack k
    case a of
        (Just v) -> return v
        Nothing  -> getGlobal k

getGlobal :: (WispMonad m) => String -> m Sexpr
getGlobal k = do
    a <- lookupGlobal k
    return $ case a of
        (Just v) -> v
        Nothing  -> error $ printf "`%s' is unbound" k -- TODO: exceptions

evalnum :: (WispMonad m, MonadStack WispVariable m) => Sexpr -> m Int
evalnum e = do
    e' <- eval e
    case e' of
        (ConstNumber n) -> return n
        _ -> error "expected number"

evalBuiltinBinary :: (WispMonad m) =>
    (Int -> Int -> Int) -> Sexpr -> Sexpr -> m Sexpr

evalBuiltinBinary f a b = do
    a' <- evalnum a
    b' <- evalnum b
    return $ ConstNumber $ a' `f` b'

eval :: (WispMonad m) => Sexpr -> m Sexpr

{------ recurse down parens ------}
eval (Paren e) = eval e
-- eval (Paren e :-: ε) = do
--     a <- eval e
--     eval $ a :-: ε
-- eval (e :-: Paren ε) = do
--     a <- eval ε
--     eval $ e :-: a

{------ builtins ------}
eval (Define k v) = do
    v' <- eval v
    setGlobal k v'
    return v'

eval (Lambda x body) = do
    st <- getStack
    return $ Closure st x body

-- eval (Lambda x body :-: arg) = do
--     eval 

eval (Closure st x body :-: arg) = do
    arg' <- eval arg
    pushRuns ((x,arg'):st) $ eval body

eval (If cond _then _else) = do
    cond' <- eval cond
    if booleanValue cond'
    then eval _then
    else eval _else

eval (Trace e) = do
    e' <- eval e
    case e' of
        (ConstNumber n) -> wispTrace n
        (ConstBool b) -> wispTrace b
        _ -> typeError
    return e'

eval (Add a b) = evalBuiltinBinary (+) a b
eval (Subtract a b) = evalBuiltinBinary (-) a b
eval (Multiply a b) = evalBuiltinBinary (*) a b
eval (Divide a b) = evalBuiltinBinary (div) a b

{------ application ------}
eval (f :-: x) = do
    f' <- eval f
    case f' of
        (Lambda _ _) -> eval $ f' :-: x
        (Closure _ _ _) -> eval $ f' :-: x
        _            -> error "attempted to apply a non-abstraction"

{------ variables ------}
eval (Identifier w) = getVar w

{------ edge case ------}
eval x = return x

{------ other shit :D ------}

booleanValue :: Sexpr -> Bool
booleanValue (ConstBool False) = False
booleanValue (ConstNumber 0) = False
booleanValue _ = True

typeError :: a
typeError = error "type error lol :3"

execProgram :: [Sexpr] -> WispIO WispVariable [Sexpr]
execProgram [] = return []
execProgram (e:es) = do
    a <- eval e
    b <- execProgram es
    return $ a : b

-- execProgram_ :: [Sexpr] -> WispIO WispVariable ()
execProgram_ [] = return ()
execProgram_ (e:es) = do
    eval e
    execProgram_ es

