import Data.List

-- This function takes in a postfix expression as a string and returns the expression in disjunction normal form
dnf :: String -> String
dnf expr = helper expr []
  where 
    helper [] stack = if length stack == 1 then head stack else concat (intersperse " v " stack)
    helper (x:xs) stack = case x of 
                            'A' -> helper xs (concat (intersperse " v " (operate 'A' (take 2 stack) : drop 2 stack))
                            'C' -> helper xs (concat (intersperse " v " (operate 'C' (take 2 stack) : drop 2 stack))
                            'D' -> helper xs (concat (intersperse " v " (operate 'D' (take 2 stack) : drop 2 stack))
                            'E' -> helper xs (concat (intersperse " v " (operate 'E' (take 2 stack) : drop 2 stack))
                            'J' -> helper xs (concat (intersperse " v " (operate 'J' (take 2 stack) : drop 2 stack))
                            'K' -> helper xs (concat (intersperse " v " (operate 'K' (take 2 stack) : drop 2 stack))
                            'N' -> helper xs (concat (intersperse " v " (operate 'N' (take 1 stack) : drop 1 stack))
                            'X' -> helper xs (concat (intersperse " v " (operate 'X' (take 2 stack) : drop 2 stack))
                            _   -> helper xs (x:stack)

-- This function takes in a postfix expression as a string and returns the expression in conjunction normal form
cnf :: String -> String
cnf expr = helper expr []
  where 
    helper [] stack = if length stack == 1 then head stack else concat (intersperse " & " stack)
    helper (x:xs) stack = case x of 
                            'A' -> helper xs (concat (intersperse " & " (operate 'A' (take 2 stack) : drop 2 stack))
                            'C' -> helper xs (concat (intersperse " & " (operate 'C' (take 2 stack) : drop 2 stack))
                            'D' -> helper xs (concat (intersperse " & " (operate 'D' (take 2 stack) : drop 2 stack))
                            'E' -> helper xs (concat (intersperse " & " (operate 'E' (take 2 stack) : drop 2 stack))
                            'J' -> helper xs (concat (intersperse " & " (operate 'J' (take 2 stack) : drop 2 stack))
                            'K' -> helper xs (concat (intersperse " & " (operate 'K' (take 2 stack) : drop 2 stack))
                            'N' -> helper xs (concat (intersperse " & " (operate 'N' (take 1 stack) : drop 1 stack))
                            'X' -> helper xs (concat (intersperse " & " (operate 'X' (take 2 stack) : drop 2 stack))
                            _   -> helper xs (x:stack)

-- This function takes in a postfix expression as a string and returns the expression in algebraic normal form
anf :: String -> String
anf expr = helper expr []
  where 
    helper [] stack = if length stack == 1 then head stack else concat (intersperse " + " stack)
    helper (x:xs) stack = case x of 
                            'A' -> helper xs (concat (intersperse " + " (operate 'A' (take 2 stack) : drop 2 stack))
                            'C' -> helper xs (concat (intersperse " + " (operate 'C' (take 2 stack) : drop 2 stack))
                            'D' -> helper xs (concat (intersperse " + " (operate 'D' (take 2 stack) : drop 2 stack))
                            'E' -> helper xs (concat (intersperse " + " (operate 'E' (take 2 stack) : drop 2 stack))
                            'J' -> helper xs (concat (intersperse " + " (operate 'J' (take 2 stack) : drop 2 stack))
                            'K' -> helper xs (concat (intersperse " + " (operate 'K' (take 2 stack) : drop 2 stack))
                            'N' -> helper xs (concat (intersperse " + " (operate 'N' (take 1 stack) : drop 1 stack))
                            'X' -> helper xs (concat (intersperse " + " (operate 'X' (take 2 stack) : drop 2 stack))
                            _   -> helper xs (x:stack)

-- This function takes in an operator and two strings and returns a string representing the expression
-- with the operator applied to the two strings
operate :: Char -> [String] -> String
operate op [x, y] = case op of 
                      'A' ->  "(" ++ x ++ " v " ++ y ++ ")"
                      'C' ->  "(" ++ x ++ " => " ++ y ++ ")"
                      'D' ->  "(" ++ x ++ " | " ++ y ++ ")"
                      'E' ->  "(" ++ x ++ " <=> " ++ y ++ ")"
                      'J' ->  "(" ++ x ++ " + " ++ y ++ ")"
                      'K' ->  "(" ++ x ++ " & " ++ y ++ ")"
                      'X' ->  "(" ++ x ++ " x " ++ y ++ ")"
                      'N' ->  "(~" ++ x ++ ")"

main :: IO ()
main = do 
    input <- getLine
    let dnf_expr = dnf input
    let cnf_expr = cnf input
    let anf_expr = anf input
    putStrLn input
    putStrLn dnf_expr
    putStrLn cnf_expr
    putStrLn anf_expr
    putStrLn "*"
    main
    return ()

-- This program takes in a postfix expression and outputs the expression in disjunction normal form, conjunction normal form and algebraic normal form. It uses a helper function, operate, which takes in an operator and two strings and returns a string representing the expression with the operator applied to the two strings.

-- The main function reads the input from the standard input stream, then calls the dnf, cnf and anf functions to get the expressions in the different normal forms. It then prints out the input expression and the expressions in the different normal forms and prints a * after each set of four lines. It then recursively calls itself to get the next input expression.