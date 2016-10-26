is_x :: Int -> Int -> Int -> Int
is_x a y z = ([x | x <- [a,a*2..a*(y-1)] , (mod x y) == z] !! 0) `div` a

create_prime_list :: Int -> Int -> [Int]
create_prime_list n x
  | n >= x     = []
  | otherwise  = if (judge_prime_num x ( x - 1) ) then create_prime_list n (x - 1) ++ [x] else create_prime_list n (x - 1)
    where judge_prime_num x 1 = True
          judge_prime_num x y = if (x `mod` y) == 0 then False else judge_prime_num x ( y - 1 )

isPrime :: Int -> Bool
isPrime n = judge_prime_num n (n-1)
  where judge_prime_num x 1 = True
        judge_prime_num x y = if (x `mod` y) == 0 then False else judge_prime_num x ( y - 1 )

prime_list :: [Int]
prime_list = [ x| x <- [4..], and [x `mod` y /= 0 | y <- [2..(floor $ sqrt $ realToFrac x)]]]
