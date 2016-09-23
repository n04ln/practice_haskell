main = do
  putStrLn "Hello, What is your name??"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")
