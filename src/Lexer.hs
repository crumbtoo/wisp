module Lexer where

import Data.Char

data Token = TokenLParen
           | TokenRParen
           | TokenIdentifier String
           | TokenNum Int
           | TokenString String
           | TokenDefine
           | TokenLambda
           | TokenIf
           | TokenPrint
           deriving Show

infixl 5 :-:

data Sexpr = Identifier String
           | ConstNumber Int
           | ConstString String
           | ConstUnit
           | Sexpr :-: Sexpr -- application
           | Paren Sexpr
           | Define String Sexpr
           | Lambda String Sexpr
           | If Sexpr Sexpr Sexpr
           | Print Sexpr
           {-------- below only arise from evaluation --------}
           | Add Sexpr Sexpr
           | Subtract Sexpr Sexpr
           | Multiply Sexpr Sexpr
           | Divide Sexpr Sexpr
           deriving Show

lexer :: String -> [Token]
lexer [] = []

{-------- operators --------}
lexer (';':cs) = lexer $ dropWhile (/='\n') cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
-- lexer ('"':cs) = lexQuote ('"':cs)

{-------- keywords --------}
lexer s
    | word == "define"   = TokenDefine : lexer rest
    | word == "lambda"   = TokenLambda : lexer rest
    | word == "if"       = TokenIf     : lexer rest
    | word == "print"    = TokenPrint  : lexer rest
    where (word,rest) = span (\c -> not $ isSpace c || c == ')' || c == '(') s

{-------- misc --------}
lexer (c:cs)
    | isInitial c = lexIdentifier (c:cs)
    | isSpace c   = lexer cs
    | isDigit c   = lexNum (c:cs)

isInitial :: Char -> Bool
isInitial ch = isLetter ch || ch `elem` identifierSymbols 

identifierSymbols :: String
identifierSymbols = "!#$%&+-*/.:<=>?@\\^_`|~"

lexIdentifier :: String -> [Token]
lexIdentifier (c:cs) = (TokenIdentifier $ c : s) : lexer rest
    where
        (s,rest) = span subsequent cs
        subsequent ch = isInitial ch || isDigit ch

lexNum :: String -> [Token]
lexNum s = TokenNum (read num) : lexer rest
    where (num,rest) = span isDigit s

lexString :: String -> [Token]
lexString (c:cs) = TokenString str : lexer rest
    where (str,(rest)) = span (/=c) cs

lexWord :: String -> [Token]
lexWord s
    | word == "define"   = TokenDefine : lexer rest
    | word == "lambda"   = TokenLambda : lexer rest
    | word == "if"       = TokenIf     : lexer rest
    where (word,rest) = span (\c -> not $ isSpace c || c == ')' || c == '(') s

