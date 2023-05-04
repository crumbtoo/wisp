{
{-# LANGUAGE CPP #-}
module Parser where

import Data.Char
import System.Environment
import System.IO
import Control.Monad
import Text.Printf
import Data.Maybe
import Lexer

}

%name parseBlock Block

%tokentype { Token }
%error { parseError }

%token
	{---- lush ----}
	'$'		  { TokenDollar }
    '$('      { TokenDollarLParen  }
    '@('      { TokenDollarLParen  }
    word      { TokenWord $$ }
	{---- shell ----}
    '>'       { TokenGreat  }
    '>>'      { TokenDGreat  }
    '<'       { TokenLess  }
    '|'       { TokenPipe  }
	{---- lua ----}
    ';'       { TokenSemicolon  }
    ','       { TokenComma  }
    '='       { TokenAssign  }
    '('       { TokenLParen  }
    ')'       { TokenRParen  }
    '+'       { TokenPlus  }
    '..'      { TokenConcat  }
    '...'     { TokenDotDotDot  }
    name      { TokenName $$ }
    and       { TokenAnd        }
    break     { TokenBreak      }
    do        { TokenDo         }
    else      { TokenElse       }
    elseif    { TokenElseif     }
    end       { TokenEnd        }
    false     { TokenFalse      }
    for       { TokenFor        }
    function  { TokenFunction   }
    goto      { TokenGoto       }
    if        { TokenIf         }
    in        { TokenIn         }
    local     { TokenLocal      }
    nil       { TokenNil        }
    not       { TokenNot        }
    or        { TokenOr         }
    repeat    { TokenRepeat     }
    return    { TokenReturn     }
    then      { TokenThen       }
    true      { TokenTrue       }
    until     { TokenUntil      }
    while     { TokenWhile      }
    numeral   { TokenNumeral $$ }
    litstring { TokenLitString $$ }

%left '+'
%left '..'

%%

Block : Stat                { [$1] }
      | Stat Retstat        { $1 : [$2] }
      | Retstat             { [$1] }
      | Stat Block          { $1 : $2 }
	  | {- empty -}			{ [] }

Stat : ';'												{ Semicolon }
     | break                    						{ Break }
     | goto name                						{ Goto $2 }
     | while Exp do Block end   						{ While $2 $4 }
	 | for name '=' Exp ',' Exp do Block end			{ For $2 $4 $6 (Numeral 1) $8 }
	 | for name '=' Exp ',' Exp ',' Exp do Block end	{ For $2 $4 $6 $8 $10 }
	 | for NameList in ExpList do Block end				{ ForIn $2 $4 $6 }
	 | local NameList									{ Local $2 [] }
	 | local NameList '=' ExpList						{ Local $2 $4 }
     | FunctionCall										{ StatFunctionCall $1 }
     | FunctionCall	RedirList							{ StatFunctionCallRedir $2 $1 }

RedirList : Redir			{ [$1] }
		  | Redir RedirList { $1 : $2 }

Redir : '>' Word			{ Great $2 }
	  | '>>' Word			{ DGreat $2 }

Retstat : return                { Return [Nil] }
        | return ';'            { Return [Nil] }
        | return ExpList        { Return $2 }
        | return ExpList ';'    { Return $2 }

ExpList : Exp                   { [$1] }
        | Exp ',' ExpList       { $1 : $3 }

Exp  : nil                  { Nil }
     | false                { LFalse }
     | true                 { LTrue }
     | numeral              { Numeral $1 }
     | litstring            { LiteralString $1 }
     | PrefixExp            { $1 }
     | Exp '+' Exp          { $1 `Plus` $3 }
     | Exp '..' Exp         { $1 `Concat` $3 }

PrefixExp : Var             { $1 }
          | '(' Exp ')'     { Paren $2 }
          | FunctionCall    { ExpFunctionCall $1 }

Var       : name            { Var $1 }

FunctionCall : PrefixExp Args   { FunctionCall $1 $2 }
             | '$(' Command ')' { $2 }

Command : Command0              { Command $1 }
	
Command0 : Word					{ [$1] }
		 | Word Command0		{ $1 : $2 }

Word : word			{ $1 }
	 | name			{ $1 }
	 | litstring	{ $1 }

Args : '(' ')'                  { [] }
     | '(' ExpList ')'          { $2 }

ParList : NameList              { $1 }

NameList : name                 { [$1] }
         | name ',' NameList    { $1 : $3 }

{

parseError :: [Token] -> a
parseError [] = error "Parse error"
parseError ts = error $ printf "Parse error near '%s'" (show $ head ts)

}

