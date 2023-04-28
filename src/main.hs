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
           | Sexpr :-: Sexpr -- application
           | Paren Sexpr
           | Define String Sexpr
           | Lambda String Sexpr
           | If Sexpr Sexpr Sexpr
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

lookupVar0 :: String -> AssocList v -> v
lookupVar0 k d = case lookup k d of
    (Just v) -> v
    Nothing -> error $ printf "`%s' is unbound" k

type WispVariable = (String,Sexpr)

lookupVar :: (Monad m, Eq k, PrintfArg k) => k -> StackT (k, v) m v
lookupVar k = do
    a <- lookupStack k
    case a of
        (Just v) -> return v
        Nothing  -> error $ printf "`%s' is unbound" k -- TODO: exceptions

eval :: (Monad m) => Sexpr -> StackT WispVariable m Sexpr

{------ recurse down parens ------}
eval (Paren e) = eval e
eval (Paren e :-: ε) = do
    a <- eval e
    eval $ a :-: ε
eval (e :-: Paren ε) = do
    a <- eval ε
    eval $ e :-: a

{------ builtins ------}
eval (Define k v) = do
    v' <- eval v
    push (k,v')
    return v

eval (Lambda x body :-: arg) = do
    arg' <- eval arg
    push (x,arg')
    eval body

{------ application ------}
eval (f :-: x) = do
    f' <- eval f
    case f' of
        (Lambda _ _) -> eval $ f' :-: x
        _            -> error "attempted to apply a non-abstraction"

{------ variables ------}
eval (Identifier w) = lookupVar w

{------ edge case ------}
eval x = return x

label :: (Show a) => String -> a -> IO ()
label s a = putStrLn $ s ++ show a

printParse :: String -> IO ()
printParse s = do
    label "tokens : " $ lexer $ s
    label "ast    : " $ wispParser . lexer $ s
    label "result : " =<< evalStackT (eval . wispParser $ lexer s) env
    return ()
    where
        env = [ ("+", Lambda "x" (Lambda "y" (Add (Identifier "x") (Identifier "y"))))
              ]

main :: IO ()
main = do
    args <- getArgs
    
    if null args then do
        s <- getContents
        printParse s
    else
        mapM_ printParse args

