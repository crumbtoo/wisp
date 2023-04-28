module StackMonad where

import Data.Functor.Identity
import Control.Applicative (liftA2)
import Control.Monad (liftM2, liftM)

type Stack e v = StackT e Identity v
newtype StackT e m v = StackT { runStackT :: [e] -> m (v, [e]) }

getStack :: (Monad m) => StackT e m [e]
getStack = StackT $ \e -> return (e, e)

lookupStack :: (Monad m, Eq k) => k -> StackT (k, v) m (Maybe v)
lookupStack k = do
    e <- getStack
    let v = lookup k e
    return v

push :: (Monad m) => e -> StackT e m ()
push e = StackT $ \es -> return ((), e:es)

instance (Monad m) => Functor (StackT e m) where
    fmap = liftM

instance (Monad m) => Applicative (StackT e m) where
    pure v = StackT $ \e -> return (v,e)
    liftA2 = liftM2

-- (>>=) runs k with the inherited env
-- return ignores it
instance (Monad m) => Monad (StackT e m) where
    (>>=) :: StackT e m a -> (a -> StackT e m b) -> StackT e m b
    m >>= k = StackT $ \e -> do
        (a, ne) <- runStackT m e
        runStackT (k a) ne

test1 :: Stack (String,String) Int
test1 = do
    k <- lookupStack "doge"
    if null k
    then push ("res","no doge...")
    else push ("res","doge!")
    return 0

