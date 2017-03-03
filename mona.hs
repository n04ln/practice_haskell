f :: Num a => Maybe a
f = return 12

g :: Num a => IO a
g  = return 12 

-- fはMaybe, gはIOのモナドに包まって返される
-- そのアクションの文脈でモナドが適宜選ばれる。

h :: (Show a) => a -> IO ()
h x = do
  print x
  return ()  -- IO () を返す

-- main = f >>= g
-- 上と下は同義
main = do
  x <- g
  h x

