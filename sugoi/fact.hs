fractorial :: Int -> Int
fractorial 0 = 1
fractorial n = n * fractorial ( n - 1 )
