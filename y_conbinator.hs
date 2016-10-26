y x = x (y x)
x = \f (x:xs) -> case xs of
                   [] -> x
                   _ -> x * f xs
product1 = y $ x

-- -- -- -- -- -- -- -- -- -- -- -- --

product2 (x:xs)
  | xs == [] = x
  | otherwise = x * (product xs)


