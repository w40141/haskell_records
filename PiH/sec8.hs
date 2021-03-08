type Pos = (Int, Int)

type Trans = Pos -> Pos

type Pair a = (a, a)

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y -1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m : ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (div m n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n -1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

-- t :: Tree Int
-- t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- occurs :: Eq a => a -> Tree a -> Bool
-- occurs x (Leaf y) = x == y
-- occurs x (Node l y r) = x == y || occurs x l || occurs x r

-- occurs :: Ord a => a -> Tree a -> Bool
-- occurs x (Leaf y) = x == y
-- occurs x (Node l y r)
--   | x == y = True
--   | x < y = occurs x l
--   | otherwise = occurs x r

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of
  LT -> occurs x l
  EQ -> True
  GT -> occurs x r

leaves :: Tree' a -> Int
leaves (Leaf' _) = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

halve :: [a] -> ([a], [a])
halve ns = splitAt (length ns `div` 2) ns

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance ns = Node' (balance nx) (balance ny)
  where
    (nx, ny) = halve ns

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  | Or Prop Prop
  | Eqiv Prop Prop

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Eqiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Eqiv p q) = vars p ++ vars q

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Int]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

-- bools :: Int -> [[Bool]]
-- bools n = map (reverse . map conv . make n . int2bin) range
--   where
--     range = [0 .. (2 ^ n) -1]
--     make n bs = take n (bs ++ repeat 0)
--     conv 0 = False
--     conv 1 = True

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where
    bss = bools (n -1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

data Expr = Val Int | Add Expr Expr | Mult Expr Expr

data Op = EVAL Expr | ADD Int

data Op' = ADD' Expr | MULT Expr | PLUS Int | TIMES Int

type Cont' = [Op']

eval'' :: Expr -> Cont' -> Int
eval'' (Val n) c = exec' c n
eval'' (Add x y) c = eval'' x (ADD' y : c)
eval'' (Mult x y) c = eval'' x (MULT y : c)

exec' :: Cont' -> Int -> Int
exec' [] n = n
exec' (ADD' n : c) m = eval'' n (PLUS m : c)
exec' (MULT n : c) m = eval'' n (TIMES m : c)
exec' (PLUS n : c) m = exec' c (n + m)
exec' (TIMES n : c) m = exec' c (n * m)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val a) = f a
folde f g (Add a b) = g (folde f g a) (folde f g b)

eva :: Expr -> Int
eva = folde (+ 0) (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)

value :: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y

type Cont = [Op]

eval' :: Expr -> Cont -> Int
eval' (Val n) c = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)

value' :: Expr -> Int
value' e = eval' e []

-- instance Eq a => Eq (Maybe a) where
--   Nothing == Nothing = True
--   (Just x) == (Just y) = x == y
--   _ == _ = False
