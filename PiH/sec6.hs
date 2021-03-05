fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : insert x ys

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib(n-1)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                   smaller = [a | a <- xs, a <= x]
                   larger  = [b | b <- xs, b > x]

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | y > x  = euclid x (y-x)
           | x > y  = euclid (x-y) y

merge :: Ord a => [a] -> [a] -> [a]
merge x      []              = x
merge []     y               = y
merge (x:xs) (y:ys)
  | x >= y      = y : merge (x:xs) ys
  | otherwise   = x : merge xs     (y:ys)

halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
    where
        half = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where
        (left, right) = halve xs
