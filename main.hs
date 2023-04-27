data Token = TokenLParen
           | TokenRParen
           | TokenWord String
           | TokenNum Int
           | TokenString String
           deriving Show

infixr 5 :-:

data Sexpr = Word String
           | Number Int
           | LString String
           | Sexpr :-: Sexpr
           | Paren Sexpr
           | Lambda String Sexpr
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (';':cs) = lexer $ dropWhile (/='\n') cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer (c:cs)
    | c == '"'  = lexStringD (c:cs)
    | c == '\'' = lexStringS (c:cs)
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | otherwise = lexWord (c:cs)

lexNum :: String -> [Token]
lexNum s = TokenNum (read num) : lexer rest
    where (num,rest) = span isDigit s

lexStringD :: String -> [Token]
lexStringD ('"':cs) = TokenString str : lexer rest
    where (str,('"':rest)) = span (/='"') cs

lexStringS :: String -> [Token]
lexStringS ('\'':cs) = TokenString str : lexer rest
    where (str,('\'':rest)) = span (/='\'') cs

lexWord :: String -> [Token]
lexWord s = TokenWord str : lexer rest
    where (str,rest) = span (\c -> not $ isSpace c || c == ')' || c == '(') s

parseError :: [Token] -> a
parseError [] = error "Parse error"
parseError ts = error $ printf "Parse error near '%s'" (show $ head ts)

falsey :: Sexpr -> Bool
falsey (Number 0) = True
falsey _ = False

type AssocList a = [(String,a)]

eval :: AssocList Sexpr -> Sexpr -> IO Sexpr
eval d (Paren e) = eval d e
eval d (Paren e :-: ε) = do
    a <- eval d e
    eval d $ a :-: ε
eval d (e :-: Paren ε) = do
    a <- eval d ε
    eval d $ e :-: a

-- builtins
eval _ (Word "lambda" :-: Word x :-: body) = -- return $ Number 31415
    return $ Lambda x body

eval d (Lambda x body :-: arg) =
    eval ((x,arg):d) body -- eval body just to reduce what we can

eval d (Word "if" :-: cond :-: cThen :-: cElse) = do
    x <- eval d cond

    return $ if not $ falsey x 
             then cThen
             else cElse

eval d (Word "def" :-: Word k :-: v :-: body) =
    eval ((k,v):d) body

eval d (Word op :-: (Number a) :-: (Number b))
    | op == "+"  = return $ Number $ a + b
    | op == "-"  = return $ Number $ a - b
    | op == "*"  = return $ Number $ a * b
    | op == "/"  = return $ Number $ a `div` b

-- recurse expr
eval d (Word op :-: a :-: b) | op `elem` ["+","-","*","/"] = do
    x <- eval d a
    y <- eval d b
    eval d $ Word op :-: x :-: y

eval d (Word k) = case lookup k d of
    (Just v) -> eval d v
    Nothing  -> error $ printf "`%s' is undefined." k

eval d (Word k :-: rest) = case lookup k d of
    (Just v) -> eval d $ v :-: rest
    Nothing  -> error $ printf "`%s' is undefined." k

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

