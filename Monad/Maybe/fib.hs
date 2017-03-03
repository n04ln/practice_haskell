import Control.Applicative

fib :: Int -> Maybe Int
fib 0 = Just 0
fib 1 = Just 1
fib n
  | n > 1 = (+) <$> fib (n-1) <*> fib(n-2)
  | otherwise = Nothing

main :: IO ()
main = do
  print $ fib 12
  print $ fib (-12)
  print $ (*) <$> [1,3,5] <*> [2,4,6]
