module Evaluater where

import Stack
import Lexer
import Text.Printf
import Control.Monad.IO.Class

class (Monad m) => WispMonad m where
    wispPrint :: (Show a) => a -> m ()

instance WispMonad (StackT e IO) where
    wispPrint = liftIO . putStrLn . show

type WispVariable = (String,Sexpr)

-- lookupVar :: (Monad m, Eq k, PrintfArg k) => k -> StackT (k, v) m v
-- lookupVar k = do
--     a <- lookupStack k
--     case a of
--         (Just v) -> return v
--         Nothing  -> error $ printf "`%s' is unbound" k -- TODO: exceptions

-- evalnum :: Sexpr -> StackT WispVariable IO Int
-- evalnum e = do
--     e' <- eval e
--     case e' of
--         (ConstNumber n) -> return n
--         _ -> error "expected number"

-- evalBuiltinBinary :: 
--     (Int -> Int -> Int) -> Sexpr -> Sexpr -> StackT WispVariable IO Sexpr

-- evalBuiltinBinary f a b = do
--     a' <- evalnum a
--     b' <- evalnum b
--     return $ ConstNumber $ a' `f` b'

-- eval :: Sexpr -> StackT WispVariable IO Sexpr

{------ recurse down parens ------}
-- eval (Paren e) = eval e
-- eval (Paren e :-: ε) = do
--     a <- eval e
--     eval $ a :-: ε
-- eval (e :-: Paren ε) = do
--     a <- eval ε
--     eval $ e :-: a

{------ builtins ------}
-- eval (Define k v) = do
--     v' <- eval v
--     push (k,v')
--     return v

-- eval (Lambda x body :-: arg) = do
--     arg' <- eval arg
--     push (x,arg')
--     eval body

-- eval (If cond _then _else) = do
--     cond' <- eval cond
--     if not $ falsey cond'
--     then eval _then
--     else eval _else

-- eval (Add a b) = evalBuiltinBinary (+) a b
-- eval (Subtract a b) = evalBuiltinBinary (-) a b
-- eval (Multiply a b) = evalBuiltinBinary (*) a b
-- eval (Divide a b) = evalBuiltinBinary (div) a b
-- eval (Print e) = do
--     return . putStrLn $ case e of
--         (ConstNumber n) -> show n
--     return ConstUnit

{------ application ------}
-- eval (f :-: x) = do
--     f' <- eval f
--     case f' of
--         (Lambda _ _) -> eval $ f' :-: x
--         _            -> error "attempted to apply a non-abstraction"

{------ variables ------}
-- eval (Identifier w) = lookupVar w

{------ edge case ------}
-- eval x = return x

-- execProgram :: [Sexpr] -> StackT WispVariable IO [Sexpr]
-- execProgram [] = return []
-- execProgram (e:es) = do
--     a <- eval e
--     b <- execProgram es
--     return $ a : b

-- execProgram_ :: [Sexpr] -> StackT WispVariable IO ()
-- execProgram_ [] = return ()
-- execProgram_ (e:es) = do
--     eval e
--     execProgram_ es

