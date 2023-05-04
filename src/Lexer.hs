module Lexer where

import Data.Char

data Token
    {---- lush ----}
    = TokenDollar
    | TokenDollarLParen
    | TokenAtLParen
    | TokenWord String
    {---- shell ----}
    | TokenGreat
    | TokenDGreat
    | TokenLess
    | TokenPipe
    {---- lua ----}
    | TokenSemicolon
    | TokenComma
    | TokenAssign
    | TokenLParen
    | TokenRParen
    | TokenPlus
    | TokenConcat
    | TokenDotDotDot
    | TokenName String
    | TokenBreak
    | TokenAnd
    | TokenDo
    | TokenElse
    | TokenElseif
    | TokenEnd
    | TokenFalse
    | TokenFor
    | TokenFunction
    | TokenGoto
    | TokenIf
    | TokenIn
    | TokenLocal
    | TokenNil
    | TokenNot
    | TokenOr
    | TokenRepeat
    | TokenReturn
    | TokenThen
    | TokenTrue
    | TokenUntil
    | TokenWhile
    | TokenNumeral Double
    | TokenLitString String
    deriving Show

type Name = String
type Block = [Stat]

data Stat = Semicolon
          | Break
          | Goto Name
          | Return [Exp]
          | While Exp Block
          | For Name Exp Exp Exp Block
          | ForIn [Name] [Exp] Block
          | StatFunctionCall FunctionCall
          | StatFunctionCallRedir [Redir] FunctionCall
          | Local [Name] [Exp]
          deriving Show

data Exp  = Nil
          | LFalse
          | LTrue
          | Numeral Double
          | LiteralString String
          | Exp `Plus` Exp
          | Exp `Concat` Exp

          {---- PrefixExp ----}
          | Var Name
          | Paren Exp
          | ExpFunctionCall FunctionCall
          deriving Show

data Redir = Great String
           | DGreat String
           deriving Show

data FunctionCall = FunctionCall Exp [Exp]
                  | Command [String]
                  deriving Show

-- officially contextful
shlexer :: String -> [Token]

-- 1. If the end of input is recognized, the current token (if any) shall be delimited.
shlexer [] = []

shlexer ('@':'(':cs) = TokenAtLParen : lexer cs
shlexer ('>':'>':cs) = TokenDGreat : shlexer cs
shlexer ('>':cs) = TokenGreat : shlexer cs
shlexer ('<':cs) = TokenLess : shlexer cs
shlexer ('|':cs) = TokenPipe : shlexer cs

-- exit shell mode
shlexer (')':cs) = TokenRParen : lexer cs

shlexer (c:cs) | isSpace c   = lexer cs

shlexer s = TokenWord word : shlexer rest
    where (word,rest) = span (\c -> c /= ' ' && c `notElem` "<>()`;&|") s


lexer :: String -> [Token]
lexer [] = []

lexer ('-':'-':cs) = lexer $ dropWhile (/='\n') cs

{-------- operators --------}
lexer ('.':'.':cs) = TokenConcat : lexer cs
lexer ('$':'(':cs) = TokenDollarLParen : shlexer cs
lexer ('$':cs) = TokenDollar : lexer cs
lexer ('>':'>':cs) = TokenDGreat : lexer cs
lexer ('>':cs) = TokenGreat : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer (';':cs) = TokenSemicolon : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('=':cs) = TokenAssign : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs

{-------- strings --------}
lexer ('"':cs) = TokenLitString str : lexer rest
    where (str,'"':rest) = span (/='"') cs

{-------- keywords --------}
lexer s
    | word  ==  "and"       =  TokenAnd       :  lexer  rest
    | word  ==  "break"     =  TokenBreak     :  lexer  rest
    | word  ==  "do"        =  TokenDo        :  lexer  rest
    | word  ==  "else"      =  TokenElse      :  lexer  rest
    | word  ==  "elseif"    =  TokenElseif    :  lexer  rest
    | word  ==  "end"       =  TokenEnd       :  lexer  rest
    | word  ==  "false"     =  TokenFalse     :  lexer  rest
    | word  ==  "for"       =  TokenFor       :  lexer  rest
    | word  ==  "function"  =  TokenFunction  :  lexer  rest
    | word  ==  "goto"      =  TokenGoto      :  lexer  rest
    | word  ==  "if"        =  TokenIf        :  lexer  rest
    | word  ==  "in"        =  TokenIn        :  lexer  rest
    | word  ==  "local"     =  TokenLocal     :  lexer  rest
    | word  ==  "nil"       =  TokenNil       :  lexer  rest
    | word  ==  "not"       =  TokenNot       :  lexer  rest
    | word  ==  "or"        =  TokenOr        :  lexer  rest
    | word  ==  "repeat"    =  TokenRepeat    :  lexer  rest
    | word  ==  "return"    =  TokenReturn    :  lexer  rest
    | word  ==  "then"      =  TokenThen      :  lexer  rest
    | word  ==  "true"      =  TokenTrue      :  lexer  rest
    | word  ==  "until"     =  TokenUntil     :  lexer  rest
    | word  ==  "while"     =  TokenWhile     :  lexer  rest

    where (word,rest) = span isLetter s

{-------- misc --------}
lexer (c:cs)
    | isInitial c = lexName (c:cs)
    | isSpace c   = lexer cs
    | isDigit c   = lexNumeral (c:cs)
    where
        isInitial ch = isLetter ch || ch == '_'

lexName :: String -> [Token]
lexName s = TokenName name : lexer rest
    where (name,rest) = span (\c -> isLetter c || isDigit c || c == '_') s

lexNumeral :: String -> [Token]
lexNumeral s = TokenNumeral (fromIntegral $ str2int word) : lexer rest
    where
        (word,rest) = span isDigit s
        str2int = foldl (\acc a -> acc * 10 + a) 0 . fmap digitToInt

