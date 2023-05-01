module WispMonad where

import Data.Map.Strict as M
import Text.Printf
import Control.Monad.IO.Class
import Control.Monad.State
import Stack as S
import Lexer
import Debug.Trace

type WispVariable = (String,Sexpr)

-- TODO: stack API should be through WispMonad
class (MonadStack WispVariable m) => WispMonad m where
    wispTrace :: String -> m ()
    setGlobal :: String -> Sexpr -> m ()
    lookupGlobal :: String -> m (Maybe Sexpr)

newtype WispIO e v = WispIO { unWispIO :: StackT e (StateT (Map String Sexpr) IO) v }
    deriving ( MonadIO, Functor, Applicative, Monad )

instance (Show e) => MonadStack e (WispIO e) where
    getStack :: WispIO e [e]
    getStack = WispIO $ S.getStack

    pushRuns :: [e] -> WispIO e v -> WispIO e v
    pushRuns e m = WispIO $ S.pushRuns e (unWispIO m)

instance WispMonad (WispIO WispVariable) where
    wispTrace = liftIO . putStrLn
    
    setGlobal k v = WispIO $ StackT $ \es -> do
        modify (\s -> insert k v s)
        return ((),es)

    lookupGlobal k = WispIO $ StackT $ \es -> do
        v <- gets (M.lookup k)
        return (v, es)

showStack s = "[ " ++ go s ++ "]"
    where
        go [] = ""
        go [x] = show x ++ "\n"
        go (x:xs) = show x ++ "\n, " ++ go xs

