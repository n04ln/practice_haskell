import Data.Char

upDiv :: Int -> Int -> Int
upDiv x y = iUpDiv x y 0
  where
    iUpDiv x y i
      | x < y = i + 1
      | otherwise = iUpDiv (x-y) y (i+1)

convertCenterToUpper :: String -> String
convertCenterToUpper s = iConvertCenterToUpper s (length s)
  where
    iConvertCenterToUpper [] _ = []
    iConvertCenterToUpper (x:xs) i = if i - (i `upDiv` 2)  == length xs
                                        then toUpper x : iConvertCenterToUpper xs i
                                        else x : iConvertCenterToUpper xs i

main :: IO()
main = do
  s <- getLine
  t <- getLine
  u <- getLine
  v <- getLine
  w <- getLine
  let ss = s:t:u:v:w:[]
   in mapM_ putStrLn (convertCenterToUpper <$> ss)
