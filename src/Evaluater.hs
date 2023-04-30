module Evaluater where

import Stack
import Lexer
import Text.Printf
import Control.Monad.IO.Class
import Debug.Trace

class (Monad m) => WispMonad m where
    wispTrace :: (Show a) => a -> m ()

instance WispMonad (StackT e IO) where
    wispTrace = liftIO . putStrLn . show

type WispVariable = (String,Sexpr)

lookupVar :: (MonadStack (String,v) m) => String -> m v
lookupVar k = do
    a <- lookupStack k
    return $ case a of
        (Just v) -> v
        Nothing  -> error $ printf "`%s' is unbound" k -- TODO: exceptions

evalnum :: (WispMonad m, MonadStack WispVariable m) => Sexpr -> m Int
evalnum e = do
    e' <- eval e
    case e' of
        (ConstNumber n) -> return n
        _ -> error "expected number"

evalBuiltinBinary :: (WispMonad m, MonadStack WispVariable m) =>
    (Int -> Int -> Int) -> Sexpr -> Sexpr -> m Sexpr

evalBuiltinBinary f a b = do
    a' <- evalnum a
    b' <- evalnum b
    return $ ConstNumber $ a' `f` b'

eval :: (WispMonad m, MonadStack WispVariable m) => Sexpr -> m Sexpr

{------ recurse down parens ------}
eval (Paren e) = eval e
eval (Paren e :-: ε) = do
    a <- eval e
    eval $ a :-: ε
eval (e :-: Paren ε) = do
    a <- eval ε
    eval $ e :-: a

{------ builtins ------}
-- eval (Define k v) = do
--     v' <- eval v
--     pushBottom (k,v')
--     return v

eval (Lambda x body :-: arg) = do
    arg' <- eval arg
    pushRun (x,arg') (pure body)

eval (If cond _then _else) = do
    cond' <- eval cond
    if booleanValue cond'
    then eval _then
    else eval _else

eval (Trace e) = do
    e' <- eval e
    case e' of
        (ConstNumber n) -> wispTrace n
        _ -> typeError
    return e'

eval (Add a b) = evalBuiltinBinary (+) a b
eval (Subtract a b) = do
    a' <- eval a
    b' <- eval b
    trace (show a' ++ " - " ++ show b') $ evalBuiltinBinary (-) a b
eval (Multiply a b) = do
    a' <- eval a
    b' <- eval b
    trace (show a' ++ " * " ++ show b') $ evalBuiltinBinary (*) a b
eval (Divide a b) = evalBuiltinBinary (div) a b

{------ application ------}
eval (f :-: x) = do
    f' <- eval f
    st <- getStack
    case f' of
        (Lambda _ _) -> trace (showStack st) $ eval $ f' :-: x
        _            -> error "attempted to apply a non-abstraction"
    where
        showStack s = "[ " ++ go s ++ "]"
        go [] = ""
        go [x] = show x ++ "\n"
        go (x:xs) = show x ++ "\n, " ++ go xs


{------ variables ------}
eval (Identifier w) = lookupVar w

{------ edge case ------}
eval x = return x

{------ other shit :D ------}

booleanValue :: Sexpr -> Bool
booleanValue (ConstNumber 0) = False
booleanValue _ = True

typeError :: a
typeError = error "type error lol :3"

execProgram :: [Sexpr] -> StackT WispVariable IO [Sexpr]
execProgram [] = return []
execProgram (e:es) = do
    a <- eval e
    b <- execProgram es
    return $ a : b

execProgram_ :: [Sexpr] -> StackT WispVariable IO ()
execProgram_ [] = return ()
execProgram_ (e:es) = do
    eval e
    execProgram_ es

