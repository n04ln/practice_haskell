main = do
  cs <- getContents
  putStr $ firstNlines 10 cs
    where
      firstNlines n cs = unlines $ take n $ lines cs
