tri_guard :: Int -> Int
tri_guard n
  | n == 0 || n == 1 = 0
  | n == 2           = 1
  | n >= 3           = tri_guard (n-1) + tri_guard (n-2) + tri_guard (n-3)

tri_pattern :: Int -> Int
tri_pattern 0 = 0
tri_pattern 1 = 0
tri_pattern 2 = 1
tri_pattern n = tri_pattern (n-1) + tri_pattern (n-2) + tri_pattern (n-3)

qadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
qadd (x1, y1) (x2, y2) 
  | y1 == y2 = (x1+x2, y1)
  | otherwise = (x1*y2+x2*y1 ,y1*y2)

qadd :: (Int, Int) -> (Int, Int) -> (Int, Int)
qadd (x1, y1) (x2, y2) 
  | y1 == y2 = (x1+x2, y1)
  | otherwise = (x1*y2+x2*y1 ,y1*y2)
