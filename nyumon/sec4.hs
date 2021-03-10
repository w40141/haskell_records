import Data.List

-- ifEvenInc n =
--   if even n
--     then n + 1
--     else n

-- ifEvenDouble n =
--   if even n
--     then n * 2
--     else n

-- ifEvenSquare n =
--   if even n
--     then n ^ 2
--     else n

ifEven myFunction x =
  if even x
    then myFunction x
    else x

inc n = n + 1

double n = 2 * n

square n = n ^ 2

ifEvenInc = ifEven inc

ifEvenSquare = ifEven square

ifEvenDouble = ifEven double

-- compareLastNames name1 name2 =
--   if lastName1 > lastName2
--     then GT
--     else
--       if lastName1 < lastName2
--         then LT
--         else
--           if firstName1 > firstName2
--             then GT
--             else
--               if firstName1 < firstName2
--                 then LT
--                 else EQ
--   where
--     lastName1 = snd name1
--     lastName2 = snd name2
--     firstName1 = fst name1
--     firstName2 = fst name2

-- compareLastNames name1 name2 = case compare (snd name1) (snd name2) of
--   GT -> GT
--   LT -> LT
--   EQ -> compareFistNames name1 name2

-- where
--   lastNameFlag = compare (snd name1) (snd name2)

-- compareFistNames name1 name2 = case compare (fst name1) (fst name2) of
--   GT -> GT
--   LT -> LT
--   EQ -> EQ

compareLastNames name1 name2 =
  if result == EQ
    then compare (fst name1) (fst name2)
    else result
  where
    result = compare (snd name1) (snd name2)

-- where
--   firstNameFlag = compare (fst name1) (fst name2)

-- addressLetter name location = nameText ++ " - " ++ location
--   where
--     nameText = fst name ++ " " ++ snd name

sfOffice :: (String, String) -> String
sfOffice name =
  if lastName < "L"
    then
      nameText
        ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else
      nameText
        ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = fst name ++ " " ++ snd name

nyOffice :: (String, String) -> String
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = fst name ++ " " ++ snd name

renoOffice :: (a, String) -> String
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV, 89523"
  where
    nameText = snd name

washingtonOffice :: (String, String) -> String
washingtonOffice name = nameText ++ " - PO Box xxx - Washington, DC, XXXXX"
  where
    nameText = fst name ++ " " ++ snd name ++ ", Esq."

getLocationFunction :: String -> (String, String) -> String
getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> washingtonOffice
  _ -> (\name -> fst name ++ " " ++ snd name)

addressLetter :: (String, String) -> String -> String
addressLetter name location = locationFunction name
  where
    locationFunction = getLocationFunction location
