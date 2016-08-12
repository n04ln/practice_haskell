trinumber :: Int -> Int
trinumber 0 = 0
trinumber n = n + trinumber (n - 1)

tetration :: Integer -> Integer -> Integer
tetration n 0 = 1
tetration n m = n ^ tetration n (m - 1)

index :: Int -> [a] -> a
index 0 (x:xs) = x
index n (x:xs) = index (n - 1) xs

even_odd ::(Integral a) => [a] -> ([a], [a])
even_odd xs = (odd, even)
  where even = [x | x <- xs , mod x 2 == 0]
        odd = [x | x <- xs , mod x 2 == 1]

insert :: (Ord a) => [a] -> a -> [a]
insert [] n = n:[]
insert (x:xs) n 
  | n < x = n:x:xs
  | otherwise = x:(insert xs n)

isort :: (Ord a) => [a] -> [a]
isort [] = []
isort (x:xs) = insert (isort xs) x 


