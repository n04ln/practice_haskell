-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
-- ex(list). 5 : [ 2 , 3 , 4] 
-- ex(List). 5 `Cons` (2 `Cons` (3 `Cons` (4 `Cons` Empty))) 
infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ xs = xs
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)
--
data TrafficLight = Red | Yellow | Blue
instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Blue == Blue = True
  _ == _ = False
instance Show TrafficLight where
  show Red = "Red Signal"
  show Yellow = "Yellow Signal"
  show Blue = "Blue Signal"
--
class YesNo a where
  yesno :: a -> Bool
instance YesNo Int where
  yesno 0 = False
  yesno _ = True
instance YesNo [a] where
  yesno [] = False
  yesno _ = True
instance YesNo Bool where
  yesno = id
instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False
instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ =True
instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesRes noRes = if yesno yesnoVal then yesRes else noRes
--

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: (Ord a) => a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right) 
  | x == a = Node x left right
  | x <  a = Node a (treeInsert x left) right
  | x >  a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x <  a = treeElem x left
  | x >  a = treeElem x right

--
