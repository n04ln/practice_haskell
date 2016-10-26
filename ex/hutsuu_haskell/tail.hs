main = do
  cs <- getContents
  putStr $ firstNlines 10 cs
    where
      firstNlines n cs = unlines $ reverse $ take n $ reverse $ lines cs
