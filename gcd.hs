gcd' :: Int -> Int -> Int
gcd' x y = if (x `mod` y) /= 0 then gcd' y (x `div` y) else x
