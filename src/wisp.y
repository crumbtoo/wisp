{
{-# LANGUAGE CPP #-}
module Parser where

import Data.Char
import System.Environment
import System.IO
import Control.Monad
import Text.Printf
import Data.Maybe
import StackMonad
import Lexer
import Evaluater

}

%name parseProgram Program
%name parseSexpr Sexpr

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
    print            { TokenPrint }

%%

Program : Sexpr                         { [ $1 ] }
        | Sexpr Program                 { $1 : $2 }

Sexpr : Constant                        { $1 }
      | identifier                      { Identifier $1 }
      | Application                     { $1 }
      | '(' define identifier Sexpr ')' { Define $3 $4 }
      | '(' lambda identifier Sexpr ')' { Lambda $3 $4 }
      | '(' if Sexpr Sexpr Sexpr ')'    { If $2 $3 $4 }
      | '(' print Sexpr ')'    { If $2 $3 $4 }

Application : '(' Sexprs ')'          { $2 }

Sexprs : Sexpr                        { $1 }
       | Sexprs Sexpr                 { $1 :-: $2 }

Constant : litnum                    { ConstNumber $1 }
         | litstring                 { ConstString $1 }
         | '(' ')'                   { ConstUnit }

{

parseError :: [Token] -> a
parseError [] = error "Parse error"
parseError ts = error $ printf "Parse error near '%s'" (show $ head ts)

}

