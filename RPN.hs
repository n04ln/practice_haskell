module RPN (solveRPN) where

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words 
  where foldingFunction (x:y:ys) "*"   = (y  * x):ys
        foldingFunction (x:y:ys) "+"   = (y  + x):ys
        foldingFunction (x:y:ys) "-"   = (y  - x):ys
        foldingFunction (x:y:ys) "/"   = (y  / x):ys
        foldingFunction (x:y:ys) "^"   = (y ** x):ys
        foldingFunction (x:ys)   "ln"  = (log x):ys
        foldingFunction xs       "sum" = [sum xs]
        foldingFunction xs       num   = read num : xs -- なんで read str :: Double とかやないねん！


