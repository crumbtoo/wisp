data Token = TokenLParen
           | TokenRParen
           | TokenIdentifier String
           | TokenNum Int
           | TokenString String
           | TokenDefine
           | TokenLambda
           | TokenIf
           deriving Show

infixl 5 :-:

data Sexpr = Identifier String
           | ConstNumber Int
           | ConstString String
           | Sexpr :-: Sexpr
           | Paren Sexpr
           | Define String Sexpr
           | Lambda String Sexpr
           | If Sexpr Sexpr Sexpr
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

parseError :: [Token] -> a
parseError [] = error "Parse error"
parseError ts = error $ printf "Parse error near '%s'" (show $ head ts)

falsey :: Sexpr -> Bool
falsey (ConstNumber 0) = True
falsey _ = False

type AssocList a = [(String,a)]

lookupVar :: String -> AssocList v -> v
lookupVar k d = case lookup k d of
    (Just v) -> v
    Nothing -> error $ printf "`%s' is unbound" k

-- TODO: reader monad?
eval :: AssocList Sexpr -> Sexpr -> IO Sexpr

{------ recurse down parens ------}
eval d (Paren e) = eval d e
eval d (Paren e :-: ε) = do
    a <- eval d e
    eval d $ a :-: ε
eval d (e :-: Paren ε) = do
    a <- eval d ε
    eval d $ e :-: a

{------ builtins ------}
eval d (Lambda x body :-: arg) = eval ((x,arg):d) body


{------ arithmetic ------}


{------ variables ------}
eval d (Identifier w) = return $ lookupVar w d

{------ edge case ------}
eval _ x = return x

label :: (Show a) => String -> a -> IO ()
label s a = putStrLn $ s ++ show a

printParse :: String -> IO ()
printParse s = do
        label "tokens : " $ lexer $ s
        label "ast    : " $ dogeParser . lexer $ s
        label "result : " =<< (eval [] . dogeParser . lexer) s
        return ()

main :: IO ()
main = do
    args <- getArgs
    
    if null args then do
        s <- getContents
        printParse s
    else
        mapM_ printParse args

