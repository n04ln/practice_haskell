reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main = do
  line <- getLine
  if null line then "a"
               else do
                 putStrLn $ reverseWords line
                 main


