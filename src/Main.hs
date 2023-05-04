{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment(getArgs)
import System.Console.GetOpt

import Lexer
import Parser
import DotWriter
import DotGraph

data Flag 
    = DotAST
    deriving (Eq, Show)

printParse :: String -> IO ()
printParse s = do
    label "tokens : " $ lexer $ s
    label "ast    : " $ parseBlock . lexer $ s
    where
        label s a = putStrLn $ s ++ show a

options :: [OptDescr Flag]
options =
    [ Option ['d']     ["ast"] (NoArg DotAST)       "generate dot graph of ast"
    ]

parseOpts :: [String] -> IO ([Flag], [String])
parseOpts argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> do
            putStrLn $ show errs
            ioError $ userError $ concat errs

main :: IO ()
main = do
    argv <- getArgs
    (flags,files) <- parseOpts argv
    s <- getContents

    if DotAST `elem` flags then
        let tree = dotBlock . parseBlock . lexer $ s
            (_,dot,_) = runDotWriter1 $ graph tree
        in putStrLn dot
    else
        printParse s


