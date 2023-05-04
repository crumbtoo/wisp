{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DotGraph
( dotBlock
, dotStat
, dotExp
, graph
) where

import Lexer

import Data.List
import Data.Functor
import Data.Functor.Identity
import Data.Foldable
import Control.Applicative
import Control.Monad
import Text.Printf
import Counter
import Control.Arrow
import DotWriter
import Debug.Trace

data DotNode = DotNode
    { nodeCode :: String
    , nodeEmbeds :: [String]
    , nodeChildren :: [DotNode]
    }
    deriving Show

numeral :: (Num a, Show a) => a -> String
numeral = show

binop :: String -> String
binop s = s

cmdsubst :: String -> DotNode
cmdsubst s = node "Command Substitution" [s'] []
    where s' = "$(" ++ s ++ ")"

name :: String -> DotNode
name s = node "Name" [quoted] []
    where quoted = "\\\"" ++ s ++ "\\\""

node :: String -> [String] -> [DotNode] -> DotNode
node = DotNode

graph :: DotNode -> DotWriter String ()
graph t = do
    write "digraph lush {"
    write "node [shape=record];"
    graph' Nothing t
    write "}"

graph' :: Maybe String -> DotNode -> DotWriter String String
graph' parent (DotNode l e c) = do
    k <- uniqueName
    write $ printf "%s [label=\"%s\"];" k label

    case parent of
        (Just p) -> write $ printf "%s -> %s;" p k
        Nothing -> return ()

    foldl (>>) (return mempty) $ fmap (graph' $ Just k) c
    return $ k

    where
        label :: String
        label = if null e
                then l
                else printf "{%s%s}" l $ concat $ fmap ('|':) e

dotBlock :: Block -> DotNode
dotBlock = node "Block" [] . fmap dotStat

dotStat :: Stat -> DotNode
dotStat (Semicolon) = node "Semicolon" [] []
dotStat (Break) = node "Break" [] []
dotStat (Goto s) = node "Goto" [s] []
dotStat (Return es) = node "Return" [] $ fmap dotExp es
dotStat (While e b) = node "While" [] [dotExp e, dotBlock b]
dotStat (For iter init bound incr body) =
    node "For" []
    [ name iter
    , dotExp init
    , dotExp bound
    , dotExp incr
    , dotBlock body
    ]
dotStat (ForIn ns es body) =
    node "ForIn" []
    [ nameList ns
    , expList es
    , dotBlock body
    ]
dotStat (StatFunctionCall _) = node "Function Call" [] []
dotStat (StatFunctionCallRedir _ _) = node "Function Call (redir)" [] []
dotStat (Local ns es) =
    node "Local" []
    [ nameList ns
    , expList es
    ]

expList :: [Exp] -> DotNode
expList = node "Expression List" [] . fmap dotExp

nameList :: [Name] -> DotNode
nameList = node "Name List" [] . fmap name

dotExp :: Exp -> DotNode
dotExp a = node "exp" [] []

