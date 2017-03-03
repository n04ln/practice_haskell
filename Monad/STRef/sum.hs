import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, modifySTRef, readSTRef)

sum' xs = runST $ do
  v <- newSTRef 0
  forM_ xs $ \i ->
    modifySTRef v (+i)
  readSTRef v

main :: IO ()
main = do
  print $ sum' [1..100]
