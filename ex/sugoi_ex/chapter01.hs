manlen :: (Int, Int) -> (Int, Int) -> Int
manlen (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

points :: Int -> [(Int, Int)]
points n = [(x, y) | x<-[(-n)..n], y<-[(-n)..n]]

mancircle :: Int -> [(Int, Int)]
mancircle n = filter (\(x, y) -> manlen (0, 0) (x, y) == n) (points n)


