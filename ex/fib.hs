fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' n = fibi n 1 0
  where
    fibi 0 a1 _ = a1
    fibi n a1 a2 = fibi (n-1) (a1+a2) a1


