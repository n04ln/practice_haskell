manlen :: (Int, Int) -> (Int, Int) -> Int
manlen (a, b) (c, d) = abs (a - c) + abs (b - d)

points :: Int -> [(Int,Int)]
points n = [(x,y) | x <- [-n..n], y <- [-n..n]]

mancircle :: Int -> [(Int, Int)]
mancircle n = foldr (\(x,y) acc -> if (manlen (0,0) (x,y)) == n then (x,y):acc else acc)  [] (points n)

