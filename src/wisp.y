{
{-# LANGUAGE CPP #-}
module Main where

import Data.Char
import System.Environment
import System.IO
import Control.Monad
import Text.Printf
import Data.Maybe
import StackMonad

}

%name dogeParser
%tokentype { Token }
%error { parseError }

%token
    '('              { TokenLParen }
    ')'              { TokenRParen }
    identifier       { TokenIdentifier $$ }
    litnum           { TokenNum $$ }
    litstring        { TokenString $$ }
    define           { TokenDefine }
    lambda           { TokenLambda }
    if               { TokenIf }

%%

Sexprs : Sexpr                        { $1 }
       | Sexprs Sexpr                 { $1 :-: $2 }

Sexpr : Constant                        { $1 }
      | identifier                      { Identifier $1 }
      | Application                     { $1 }
      | '(' lambda identifier Sexpr ')' { Lambda $3 $4 }
      | if Sexpr Sexpr Sexpr            { If $2 $3 $4 }

Application : '(' Sexprs ')'          { $2 }

Constant : litnum                    { ConstNumber $1 }
         | litstring                 { ConstString $1 }

{
#include "../src/main.hs"
}
