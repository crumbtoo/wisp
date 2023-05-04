module Counter where

import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

class (Monad m) => MonadCounter m where
    uniqueN :: m Int

data CounterT m a = CounterT { runCounterT :: Int -> m (Int,a) }

{---- CounterT eval/exec ----}

evalCounterT :: (Monad m) => CounterT m a -> Int -> m Int
evalCounterT a n = fst <$> runCounterT a n

execCounterT :: (Monad m) => CounterT m a -> Int -> m a
execCounterT a n = snd <$> runCounterT a n

{---- Counter eval/exec ----}

type Counter a = CounterT Identity a

runCounter :: Counter a -> Int -> (Int, a)
runCounter a n = runIdentity $ runCounterT a n

evalCounter :: Counter a -> Int -> Int
evalCounter a n = fst $ runCounter a n

execCounter :: Counter a -> Int -> a
execCounter a n = snd $ runCounter a n

{---- CounterT Instances ----}

instance (Monad m) => Functor (CounterT m) where
    fmap :: (a -> b) -> CounterT m a -> CounterT m b
    fmap f a = CounterT $ \n -> do
        z <- execCounterT a n
        pure (n, f z)
            
instance (Monad m) => Applicative (CounterT m) where
    pure a = CounterT $ \n -> pure (n,a)

    mf <*> a = CounterT $ \n -> do
        (n',f) <- runCounterT mf n
        z <- execCounterT a n
        pure (n', f z)

instance (Monad m) => Monad (CounterT m) where
    m >>= k = CounterT $ \n -> do
        (n',a) <- runCounterT m n
        runCounterT (k a) n'

instance MonadTrans CounterT where
    lift :: (Monad m) => m a -> CounterT m a
    lift ma = CounterT $ \n -> do
        a <- ma
        return (n,a)

instance (Monad m) => MonadCounter (CounterT m) where
    uniqueN = CounterT $ \n -> pure (succ n, n)

instance (MonadIO m) => MonadIO (CounterT m) where
    liftIO a = lift $ liftIO a

