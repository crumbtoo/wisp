{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Maybe ( fromMaybe )
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.Exit
import Control.Monad
import Control.Monad.State
import Data.Map (fromList)
import Control.Monad.IO.Class

import Evaluater
import Lexer
import Parser
import Stack
import WispMonad (WispVariable, unWispIO)

data Flag 
    = Version 
    | EvalExpression String
    | ExecProgram String
    deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['V']     ["version"]      (NoArg Version)      
        "show version number"
    , Option ['e']     ["expression"]   (ReqArg EvalExpression "SEXPR")
        "evaluate expression"
    , Option ['c']     ["program"]   (ReqArg ExecProgram "PROG")
        "execute program"
    ]

printParse :: String -> IO ()
printParse s = do
    label "tokens : " $ lexer $ s
    label "ast    : " $ parseProgram . lexer $ s
    label "result : " =<< (runProgram $ parseProgram . lexer $ s)
    where
        label s a = putStrLn $ s ++ show a

runProgram :: [Sexpr] -> IO [Sexpr]
runProgram e = evalStateT
    (evalStackT (unWispIO $ execProgram e) defaultEnv) (fromList [])

-- TODO: is name collision a concern?
defaultEnv :: [WispVariable]
defaultEnv = [ ("+", mkbinop Add)
             , ("-", mkbinop Subtract)
             , ("*", mkbinop Multiply)
             , ("/", mkbinop Divide)
             , ("trace", Lambda "s" $ Trace $ Identifier "s")
             ]
    where mkbinop f = Lambda "x" (Lambda "y" (f (Identifier "x") (Identifier "y")))
        

wispOpts :: [String] -> IO ([Flag], [String])
wispOpts argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> do
            putStrLn $ show errs
            ioError (userError (concat errs ++ usageInfo header options))
   where header = "Usage: wisp [OPTION...] files..."

main :: IO ()
main = do
    (opts,files) <- getArgs >>= wispOpts
    return ()

{--}
    if null opts && null files then
        getContents >>= printParse
    else
        forM_ files (\f -> do
            s <- readFile f
            -- evalStackT (execProgram_ $ parseProgram $ lexer s) defaultEnv
            return ()
            )
--}
        
