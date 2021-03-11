-- myGcd a b =
--   if remainder == 0
--     then b
--     else myGcd b remainder
--   where
--     remainder = mod a b

myGcd a 0 = a
myGcd a b = myGcd b (mod a b)

-- sayAmount n = case n of
--                 1 -> "one"
--                 2 -> "two"
--                 3 -> "three"

sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount 3 = "three"

isEmpty [] = True
isEmpty aList = False

myHead (x:xs) = x
myHead [] = error "No head for empty list"

myTail (_:xs) = xs
myTail [] = []
