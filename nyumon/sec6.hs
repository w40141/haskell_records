isPalindrome word = word == reverse word

respond phrase =
  if '!' `elem` phrase
    then "wow!"
    else "uh.. okay"

takeLast n aList = reverse (take n (reverse aList))

ones n = take n (cycle [1])

assignToGroups n aList = zip groups aList
  where
    groups = cycle [1 .. n]

repeat' x = cycle [x]

subseq m n aList = reverse (drop n (reverse (drop m aList)))

inFirstHalf n aList = elem n (take (div (length aList) 2) aList)
