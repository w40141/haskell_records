-- sumSquareOrSquareSum x y =
--   if sumSquare > squareSum
--     then sumSquare
--     else squareSum
--   where
--     sumSquare = x ^ 2 + y ^ 2
--     squareSum = (x + y) ^ 2

-- body sumSquare squareSum =
--   if sumSquare > squareSum
--     then sumSquare
--     else squareSum

-- sumSquareOrSquareSum x y = body (x ^ 2 + y ^ 2) ((x + y) ^ 2)

-- body = (\sumSquare squareSum -> if sumSquare > squareSum then sumSquare else squareSum)

sumSquareOrSquareSum x y =
  let sumSquare = (x ^ 2 + y ^ 2)
      squareSum = (x + y) ^ 2
   in if sumSquare > squareSum
        then sumSquare
        else squareSum

-- doubleDouble x = dubs * 2
--   where
--     dubs = x * 2

doubleDouble x = (* 2) (x * 2)

-- overwrite x = let x = 2
--                in
--                let x = 3
--                 in
--                 let x = 4
--                  in x

overwrite x =
  ( \x ->
      ( \x ->
          (\x -> x)
            4
      )
        3
  )
    2

counter x = (\x -> x + 1) ((\x -> x + 1) ((\x -> x) x))
