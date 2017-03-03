main = do
  x <- getLine 
  y <- getLine
  let x' = read x :: Int
      y' = read y :: Int
   in print (x' + y')

