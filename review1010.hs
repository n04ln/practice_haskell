lazy :: Int -> Int
lazy x = 
  let y = x + z
      z = 10
   in x + y + z


-- H-Midterm Exam

-- Q4: A. 1:2:3:4:5:[], take 5 [1,2..]

-- Q5: リストは全部同じ型でなければならない。タプル（ペア）は異なる方でも良い
list2nd (x:y:ys) = y
tuple2nd (x,y) = y

-- Q9
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Q10
max' :: (Ord a) => a -> a -> a
max' x y = if x > y then x else y

-- Q11
maximum' :: (Ord a) => [a] -> Maybe a
maximum' [] = Nothing
maximum' xs = Just (foldl1 max' xs)

-- Q12
replicate' :: Int -> a -> [a]
replicate' 1 x = [x]
replicate' n x = x : replicate' (n - 1) x

-- Q13
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] [] = []
zipWith' f [] ys = []
zipWith' f xs [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Q14
q14a = map (^2) [x | x<-[1..100], odd x]

-- Q15
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- Q16
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x then x : (filter' p xs) else filter' p xs

-- Q17
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = (\x y -> f y x)

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = g
  where g y x = f x y

-- Q18
sum' xs = foldl (+) 0 xs

-- Q19
reverse' xs = foldl (flip (:)) [] xs

