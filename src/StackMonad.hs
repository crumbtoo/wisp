{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}

module Stack where

import Data.Functor.Identity
import Control.Applicative (liftA2)
import Control.Monad (liftM2, liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

{-------- MonadStack --------}

class (Monad m) => MonadStack e m | m -> e where
    getStack :: (Monad m) => m [e]
    pushRuns :: (Monad m) => [e] -> m v -> m v

    pushRun :: (Monad m) => e -> m v -> m v
    pushRun e m = pushRuns [e] m

lookupStack :: (MonadStack (k,v) m, Eq k) => k -> m (Maybe v)
lookupStack k = do
    e <- getStack
    return $ lookup k e

{-------- StackT --------}

type Stack e v = StackT e Identity v
newtype StackT e m v = StackT { runStackT :: [e] -> m (v, [e]) }

evalStackT :: (Monad m) => StackT e m v -> [e] -> m v
evalStackT m e = fmap fst $ runStackT m e

instance (Monad m) => Functor (StackT e m) where
    fmap = liftM

instance (Monad m) => Applicative (StackT e m) where
    pure v = StackT $ \es -> return (v,es)
    liftA2 = liftM2

-- (>>=) runs k with the inherited env
-- return ignores it
instance (Monad m) => Monad (StackT e m) where
    (>>=) :: StackT e m a -> (a -> StackT e m b) -> StackT e m b
    m >>= k = StackT $ \e -> do
        (a, ne) <- runStackT m e
        runStackT (k a) e

instance (Monad m) => MonadStack e (StackT e m) where
    getStack :: StackT e m [e]
    getStack = StackT $ \e -> return (e, e)

    pushRuns :: [e] -> StackT e m v -> StackT e m v
    pushRuns e st = StackT $ \es -> runStackT st (e ++ es)

instance MonadTrans (StackT e) where
    lift :: (Monad m) => m a -> StackT e m a
    lift ma = StackT $ \e -> do
        a <- ma
        return (a,e)

instance (MonadIO m) => MonadIO (StackT e m) where
    liftIO a = lift $ liftIO a

