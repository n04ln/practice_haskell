sort ::(Ord a) =>  [a] -> [a]
sort [] = []
sort (x:xs) = sort smaller ++ [x] ++ sort larger 
  where
    larger = [a | a <- xs, a > x]
    smaller = [a | a <- xs, a <= x]

