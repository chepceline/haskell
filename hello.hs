data PropLogic = Literal Char 
               | Not PropLogic 
               | Conj PropLogic PropLogic 
               | Disj PropLogic PropLogic 
               | Implies PropLogic PropLogic 
               | Equiv PropLogic PropLogic 
               | Xor PropLogic PropLogic
 
-- read the input string and convert it to a PropLogic expression
readPropLogic :: String -> PropLogic
readPropLogic = fst . head . foldl toPropLogic ([], [])
  where
    toPropLogic (stack@(s:ss), ls) (x:xs)
      | x == 'P' = (Literal x : stack, ls)
      | x == 'N' = (Not s : ss, ls)
      | x == 'K' = (Conj s (head ss) : (tail ss), ls)
      | x == 'A' = (Implies (head ss) s : (tail ss), ls)
      | x == 'E' = (Equiv (head ss) s : (tail ss), ls)
      | x == 'J' = (Xor (head ss) s : (tail ss), ls)
      | x == 'D' = (Disj (head ss) s : (tail ss), ls)
    toPropLogic (stack, ls) (x:xs) = (stack, x:xs:ls)
 
-- convert a PropLogic expression to negation normal form
nnf :: PropLogic -> PropLogic
nnf (Literal x) = Literal x
nnf (Not (Literal x)) = Not (Literal x)
nnf (Not (Not p)) = nnf p
nnf (Conj p q) = Conj (nnf p) (nnf q)
nnf (Disj p q) = Disj (nnf p) (nnf q)
nnf (Implies p q) = Disj (nnf (Not p)) (nnf q)
nnf (Equiv p q) = Conj (Disj (nnf p) (nnf (Not q))) (Disj (nnf (Not p)) (nnf q))
nnf (Xor p q) = Disj (Conj (nnf p) (nnf (Not q))) (Conj (nnf (Not p)) (nnf q))
 
-- convert a PropLogic expression in negation normal form to conjunctive normal form
cnf :: PropLogic -> PropLogic
cnf (Literal x) = Literal x
cnf (Not (Literal x)) = Not (Literal x)
cnf (Conj p q) = Conj (cnf p) (cnf q)
cnf (Disj p q) = Disj (cnf p) (cnf q)
cnf (Not (Conj p q)) = Disj (cnf (Not p)) (cnf (Not q))
cnf (Not (Disj p q)) = Conj (cnf (Not p)) (cnf (Not q))
 
-- determine if a propositional formula in conjunctive normal form is a tautology
isTautology :: PropLogic -> Bool
isTautology (Literal x) = False
isTautology (Not (Literal x)) = False
isTautology (Conj p q) = isTautology p && isTautology q
isTautology (Disj p q) = isTautology p || isTautology q
isTautology (Not p) = False
 
-- main function to do the computation
main :: IO ()
main = do
  input <- getLine
  let expr = readPropLogic input
  let nnfExpr = nnf expr
  let cnfExpr = cnf nnfExpr
  let result = isTautology cnfExpr
  putStrLn $ show result ++ " " ++ show cnfExpr
  main
