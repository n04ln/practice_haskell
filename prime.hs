create_prime_list :: Int -> [Int]
create_prime_list = reverse . rev_create_prime_list 
  where rev_create_prime_list 1 = []
        rev_create_prime_list x  = if (judge_prime_num x ( x - 1) ) then x:rev_create_prime_list (x - 1) else rev_create_prime_list (x - 1)
          where judge_prime_num x 1 = True
                judge_prime_num x y = if (x `mod` y) == 0 then False else judge_prime_num x ( y - 1 )

create_prime_list' :: Int -> [Int]
create_prime_list' 1 = []
create_prime_list' x  = if (judge_prime_num x ( x - 1) ) then create_prime_list' (x - 1) ++ [x] else create_prime_list' (x - 1)
  where judge_prime_num x 1 = True
        judge_prime_num x y = if (x `mod` y) == 0 then False else judge_prime_num x ( y - 1 )

primes :: Int -> [Integer]
primes x = take x primesAll

primesAll :: [Integer]
primesAll = sieve [2..]
  where
    sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]

int2bin :: Int -> [Int]
int2bin = reverse . rev_int2bin
  where rev_int2bin 0 = []
        rev_int2bin x = x `mod` 2 : rev_int2bin ( x `div` 2 )

