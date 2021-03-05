import Data.Char
import Data.List

type Bit = Int

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^ 2) (filter even ns))

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

compluteParity :: [Bit] -> Int
compluteParity bits = sum bits `mod` 2

addParityBit :: [Bit] -> [Bit]
addParityBit bits = compluteParity bits : bits

checkParityBit :: [Bit] -> [Bit]
checkParityBit (x : xs)
  | x == compluteParity xs = xs
  | otherwise = error "Parity error"

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
-- chop8 [] = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)
chop8 = unfold null (take 8) (drop 8)

encode' :: String -> [Bit]
encode' = concat . map (addParityBit . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

decode' :: [Bit] -> String
decode' = map (chr . bin2int . checkParityBit) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

faultyTransmit :: String -> String
faultyTransmit = decode' . faultyChannel . encode'

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots =
  [ ["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]
  ]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
  [c] -> c
  (c : cs) -> winner' (elim c bs)

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (n : ns)
  | p n = n : takeWhile' p ns
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (n : ns)
  | p n = dropWhile' p ns
  | otherwise = n : ns

map' :: (a -> b) -> [a] -> [b]
-- map' f = foldr (\x xs -> f x : xs) []
map' f = unfold null (f . head) tail

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\n ns -> n * 10 + ns) 0

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y

iterat :: (a -> a) -> a -> [a]
iterat = unfold (const False) id

luhnDouble :: Int -> Int
luhnDouble n = if x < 10 then x else x - 9
  where x = n * 2

checkluhn :: Int -> Bool
checkluhn x = mod x 10 == 0

luhn :: [Int] -> Bool
luhn = checkluhn . sum . altMap luhnDouble id

-- luhn :: Num a => [a] -> a
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g [x] = [f x]
altMap f g (x : y : xs) = f x : g y : altMap f g xs
