myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x : xs) = x : myTake (n -1) xs

-- myTake n (x:xs) = x: rest
--     where rest = myTake (n-1) xs

finiteCycle (first : rest) = first : rest ++ [first]

myCycle (first : rest) = first : myCycle (rest ++ [first])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m -1) 1
ackermann m n = ackermann (m -1) (ackermann m (n -1))

collatz 1 = 1
collatz n =
  if even n
    then 1 + collatz (div n 2)
    else 1 + collatz (n * 3 + 1)

myReverse [] = []
myReverse [n] = [n]
myReverse (n : ns) = myReverse ns ++ [n]

fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

fastFib n m 0 = m - n
fastFib n m c = fastFib m (n+m) (c-1)
