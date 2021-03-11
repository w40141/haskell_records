import Data.Char
-- addAnA [] = []
-- addAnA (x:xs) = ("a " ++ x): addAnA xs
addAnA = map ("a " ++)

myFilter test [] = []
myFilter test (x : xs) =
  if test x
    then x : myFilter test xs
    else myFilter test xs

myProduct xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^ 2) xs)

rcons x y = y : x

myReverse xs = foldl rcons [] xs

myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInti xs
  where
    newInti = f init x

myFoldr f init [] = init
myFoldr f init (x : xs) = f x rightResult
  where
    rightResult = myFoldr f init xs

myElem n xs = length (filter (== n) xs) /= 0

-- isPalindrome word = word == reverse word

isPalindrome word = reverse tmpWord == tmpWord
  where rWord = filter (/=' ') word
        tmpWord = map toLower rWord

harmonic n = foldl (+) 0 (map (1 / ) [1 .. n])
