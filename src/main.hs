{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Maybe ( fromMaybe )
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.Exit
import Control.Monad

import Evaluater
import Lexer
import Parser
import StackMonad

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
    label "result : " =<< evalStackT (execProgram . parseProgram $ lexer s) defaultEnv
    return ()
    where
        label s a = putStrLn $ s ++ show a

defaultEnv :: [WispVariable]
defaultEnv = [ ("+", mkbinop Add)
             , ("-", mkbinop Subtract)
             , ("*", mkbinop Multiply)
             , ("/", mkbinop Divide)
             , ("print", Lambda "s" $ Print $ Identifier "s")
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

    if null opts && null files then
        getContents >>= printParse
    else
        forM_ opts (\case
            (Version) -> putStrLn "wisp 0.0.0" >> exitSuccess
            (ExecProgram s) ->
                printParse s
                -- putStrLn . show $ evalStackT (execProgram . parseProgram $ lexer s) defaultEnv
            -- (EvalExpression s) -> do
                -- return $ evalStackT (eval . parseSexpr $ lexer s) defaultEnv
                -- return ()
            )
        
