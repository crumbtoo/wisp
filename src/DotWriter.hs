module DotWriter
( DotWriter
, runDotWriter
, runDotWriter1
, uniqueName
, getN
, write
) where

import Text.Printf

data DotNode = DotNode
    { nodeName :: String
    , nodeEmbeds :: [String]
    , nodeChildren :: [DotNode]
    }
    deriving Show

newtype DotWriter w a = DotWriter { runDotWriter :: Int -> (Int,w,a) }

runDotWriter1 :: (Monoid w) => DotWriter w a -> (Int,w,a)
runDotWriter1 = flip runDotWriter 0

thd :: (a, b, c) -> c
thd (_,_,c) = c

instance (Monoid w) => Functor (DotWriter w) where
    fmap f a = DotWriter $ \n ->
        ( n
        , mempty
        , f $ thd $ runDotWriter a n
        )

instance (Monoid w) => Applicative (DotWriter w) where
    pure a = DotWriter $ \n -> (n,mempty,a)

    (<*>) :: DotWriter w (a -> b) -> DotWriter w a -> DotWriter w b
    k <*> m = DotWriter $ \n ->
        ( n
        , mempty
        , thd (runDotWriter k n) $ thd (runDotWriter m n)
        )

instance (Monoid w) => Monad (DotWriter w) where
    (>>=) :: DotWriter w a -> (a -> DotWriter w b) -> DotWriter w b
    m >>= k = DotWriter $ \n ->
        let (n',w,z) = runDotWriter m n
        in mapSndOf3 (w<>) $ runDotWriter (k z) n'

        where mapSndOf3 f (a,b,c) = (a, f b, c)

uniqueName :: (Monoid w) => DotWriter w String
uniqueName = DotWriter $ \n -> (succ n, mempty, printf "n%d" n)

getN :: (Monoid w) => DotWriter w Int
getN = DotWriter $ \n -> (n, mempty, n)

node :: String -> DotWriter String ()
node label = do
    k <- uniqueName
    write $ printf "%s [label=\"%s\"];" k label

write :: a -> DotWriter a ()
write a = DotWriter $ \n ->
    ( n    -- don't increment counter
    , a         -- write monoid
    , ()        -- no result
    )

test0 = do
    node "doge"
    node "soge"

test :: DotWriter String ()
test = do
    test0
    node "doge"
    node "quoge"
    test0
    return ()

