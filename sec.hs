-- p = ??
-- q = ??
n = p * q
w = (p - 1) * (q - 1)
-- e = ??
is_x :: Integer -> Integer -> Integer -> [Integer]
-- is_x a y z = ([x | x <- [a,a*2..a*(y-1)] , (mod x y) == z] !! 0) `div` a
is_x a y z = [x | x <- [a,a*2..a*(y-1)] , (mod x y) == z]

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
prime_list = [ x| x <- [2..], and [x `mod` y /= 0 | y <- [2..(floor $ sqrt $ realToFrac x)]]]

euclidEx a b  = f a b 1 0 0 1
  where f r 0  a  _  b  _ = ((a, b), r)
        f x y a0 a1 b0 b1 = f y r a1 (a0-q*a1) b1 (b0-q*b1)
          where (q, r) = divMod x y


