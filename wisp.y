{
{-# LANGUAGE CPP #-}
module Main where

import Data.Char
import System.Environment
import System.IO
import Control.Monad
import Text.Printf
import Data.Maybe

}

%name dogeParser
%tokentype { Token }
%error { parseError }

%token
    '('              { TokenLParen }
    ')'              { TokenRParen }
    word             { TokenWord $$ }
    litnum           { TokenNum $$ }
    litstring        { TokenString $$ }

%%

Sexpr  : word            { Word $1 }
       | litnum          { Number $1 }
       | litstring       { LString $1 }
       | Sexpr Sexpr     { $1 :-: $2 }
       | '(' Sexpr ')'   { Paren $2 }

{
#include "main.hs"
}
