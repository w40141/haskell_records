-- calChange :: Num a => a -> a -> a
calChange owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed

inc :: Num a => a -> a
inc x = x + 1

double :: Num a => a -> a
double n = 2 * n

square :: Num a => a -> a
square n = n * n

calTmp :: Integral a => a -> a
calTmp n =
  if even n
    then n - 2
    else 3 * n + 1
