data Token = TokenLParen
           | TokenRParen
           | TokenWord String
           | TokenNum Int
           | TokenString String
           | TokenDefine
           | TokenLambda
           | TokenIf
           deriving Show

infixr 5 :-:

data Sexpr = Word String
           | Number Int
           | LString String
           | Sexpr :-: Sexpr
           | Paren Sexpr
           | Define String Sexpr
           | Lambda String Sexpr
           | If Sexpr Sexpr Sexpr
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (';':cs) = lexer $ dropWhile (/='\n') cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer (c:cs)
    | c == '"' || c == '\'' = lexString c (c:cs)
    | isSpace c = lexer cs
    | isDigit c = lexNum (c:cs)
    | otherwise = lexWord (c:cs)

lexNum :: String -> [Token]
lexNum s = TokenNum (read num) : lexer rest
    where (num,rest) = span isDigit s

lexString :: Char -> String -> [Token]
lexString q s = TokenString str : lexer rest
    where (str,(_:rest)) = span (/=q) $ tail s

lexWord :: String -> [Token]
lexWord s
    | word == "define"   = TokenDefine : lexer rest
    | word == "lambda"   = TokenLambda : lexer rest
    | word == "if"       = TokenIf     : lexer rest
    | otherwise          = TokenWord word : lexer rest
    where (word,rest) = span (\c -> not $ isSpace c || c == ')' || c == '(') s

parseError :: [Token] -> a
parseError [] = error "Parse error"
parseError ts = error $ printf "Parse error near '%s'" (show $ head ts)

falsey :: Sexpr -> Bool
falsey (Number 0) = True
falsey _ = False

type AssocList a = [(String,a)]

eval :: AssocList Sexpr -> Sexpr -> IO Sexpr

{------ top ------}
eval d (Paren e) = eval d e
eval d (Paren e :-: ε) = do
    a <- eval d e
    eval d $ a :-: ε
eval d (e :-: Paren ε) = do
    a <- eval d ε
    eval d $ e :-: a

{------ builtins ------}
-- evaluate everything following with `k` bound to `v`
eval d (Define k v :-: rest) = eval ((k,v):d) rest

-- beta reduction; evaluate the body with `x` bound to `arg`
eval d (Lambda x body :-: arg) = eval ((x,arg):d) body

-- if `cond` doesn't evaluate to zero, evaluate to `then_`
eval d (If cond then_ else_) = do
    c <- eval d cond
    if not $ falsey c
    then eval d then_
    else eval d else_

{------ arithmetic ------}
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

{------ variables ------}
eval d (Word k) = case lookup k d of
    (Just v) -> eval d v
    Nothing  -> error $ printf "`%s' is undefined." k

eval d (Word k :-: rest) = case lookup k d of
    (Just v) -> eval d $ v :-: rest
    Nothing  -> error $ printf "`%s' is undefined." k


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

