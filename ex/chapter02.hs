tri_pattern :: Int -> Int
tri_pattern 0 = 0
tri_pattern 1 = 0
tri_pattern 2 = 1
tri_pattern n = tri_pattern (n - 1) + tri_pattern (n - 2) + tri_pattern (n - 3)

tri_guard :: Int -> Int
tri_guard n
  |n==0 = 0
  |n==1 = 0
  |n==2 = 1
  |otherwise = tri_guard (n - 1) + tri_guard (n - 2) + tri_guard (n - 3)

tri_case :: Int -> Int
tri_case n = 
  case n of 0 -> 0
            1 -> 0
            2 -> 1
            _ -> tri_case (n - 1) + tri_case (n - 2) + tri_case (n - 3)

qadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
qadd (p, q) (r, s) = (p*s+q*r, q*s)

qequal :: (Int, Int) -> (Int, Int) -> Bool
qequal (p, q) (r, s) = p*s == r*q

qlist :: (Int, Int) -> [(Int, Int)]
qlist (p, q) | q < 0 = qlist (-p, -q)
qlist (p, q) = [(s*p'*n, s*q'*n) | n <- [1..], s <- [1,-1]]
  where
    g = gcd p q
    p' = div p g
    q' = div q g

