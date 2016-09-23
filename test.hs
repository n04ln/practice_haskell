import Control.Monad
main = do
    s <- replicateM 3 getLine -- input: 123\n456\n789
    putStrLn . show . foldl (+) 0 . map read $ s -- output:1368
