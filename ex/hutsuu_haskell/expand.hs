main = do
  cs <- getContents
  putStr $ expand cs

expand :: String -> String
expand cs = concat $ map translate cs

translate :: Char -> String
translate c = if c == '\t' then "        " else [c]
