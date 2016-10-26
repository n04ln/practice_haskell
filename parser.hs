import Control.Exception
import Data.Char

parseTest p s = do
  print $ p s
  `catch` \(SomeException e) ->
    putStr $ show e

anyChar (x:xs)  = (x,xs)
satisfy f (x:xs) | f x = (x, xs)

char c = satisfy (== c)
digit  = satisfy isDigit
letter = satisfy isLetter

test1 xs0 =
  let (x1, xs1) = anyChar xs0
      (x2, xs2) = anyChar xs1
   in ([x1, x2], xs2)

test2 xs0 =
  let (x1, xs1) = test1 xs0
      (x2, xs2) = anyChar xs1
   in (x1 ++ [x2], xs2)

test3 xs0 =
  let (x1, xs1) = letter xs0
      (x2, xs2) = digit xs1
      (x3, xs3) = digit xs2
   in ([x1,x2,x3], xs3)

main = do
  parseTest anyChar "abc"
  parseTest test1 "abc"
  parseTest test2 "abc"
  parseTest test2 "12"
  parseTest test2 "123"
  parseTest (char 'a') "abc"
  parseTest (char 'a') "123"
  parseTest digit "abc"
  parseTest digit "123"
  parseTest letter "abc"
  parseTest letter "123"
  parseTest test3 "abc"
  parseTest test3 "123"
  parseTest test3 "a23"
  parseTest test3 "a234"
