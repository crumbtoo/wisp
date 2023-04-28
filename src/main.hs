printParse :: String -> IO ()
printParse s = do
    label "tokens : " $ lexer $ s
    label "ast    : " $ wispParser . lexer $ s
    label "result : " =<< evalStackT (execProgram . wispParser $ lexer s) env
    return ()
    where
        label s a = putStrLn $ s ++ show a

        mkbinop f = Lambda "x" (Lambda "y" (f (Identifier "x") (Identifier "y")))
        env = [ ("+", mkbinop Add)
              , ("-", mkbinop Subtract)
              , ("*", mkbinop Multiply)
              , ("/", mkbinop Divide)
              ]

main :: IO ()
main = do
    args <- getArgs
    
    if null args then do
        s <- getContents
        printParse s
    else
        mapM_ printParse args

