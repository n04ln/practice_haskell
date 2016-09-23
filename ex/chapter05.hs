applyPair :: (a -> b) -> (a, a) -> (b, b)
applyPair f (x,y) = (f x, f y)

applyN :: (a -> a) -> Int -> a -> a
applyN f 0 x = x
applyN f n x = applyN f (n - 1) (f x)

squares :: Int -> [Int]
squares n = [x^2 | x <- [1..n], x^2 <= n]

fromBinary :: [Int] -> Int
fromBinary = foldl (\acc x -> x + acc*2) 0

tails :: [a] -> [[a]]
tails = scanr (:) []

-- (1) pointFree1という名前でポイントフリースタイルで再定義せよ
pointed1 :: [Int] -> [Int]
pointed1 xs = map negate (map (+10) (filter (>0) xs))

pointFree1 :: [Int] -> [Int]
pointFree1 = map (negate.(+ 10)).filter (>0)

-- (2) pointFree2という名前でポイントフリースタイルで再定義せよ
pointed2 :: [[Int]] -> [Int]
pointed2 xss = scanl (+) 0 (map (foldl (*) 1) (filter (\xs -> length xs >= 2) xss))

pointFree2 :: [[Int]] -> [Int]
pointFree2 = scanl (+) 0 . map (foldl (*) 1) . filter((>=2) . length)

-- (3) pointFree3という名前でポイントフリースタイルで再定義せよ
-- ヒント:flip及び($)を用いよ
pointed3 :: [a -> a] -> a -> a
pointed3 fs x = foldl (\x f -> f x) x fs

pointFree3 :: [a -> a] -> a -> a
pointFree3 = flip $ foldl $ flip ($)

-- (4) pointFree3という名前でポイントフリースタイルで再定義せよ
-- ヒント:(.)をセクションで用いよ
pointed4 :: (a -> [b]) -> [a] -> [b]
pointed4 f xs = concat (map f xs)

-- (5) pointFree5という名前でポイントフリースタイルで再定義せよ
pointed5 :: (Int -> [Int]) -> [Int] -> [Int]
pointed5 f xs = foldl (\ys g -> g ys) xs (replicate 3 (\zs -> concat (map f zs)))

church :: Int -> (a -> a) -> a -> a
church 0 f z = z
church n f z = f  $ church (n - 1) f z

unchurch c = c (+1) 0

csucc c f z = f $ c f z

cadd c1 c2 f z = c1 f (c2 f z)

