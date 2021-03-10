ifEven myFunction x =
  if even x
    then myFunction x
    else x

inc n = n + 1

double n = 2 * n

square n = n ^ 2

-- ifEvenInc = ifEven inc

-- ifEvenSquare = ifEven square

-- ifEvenDouble = ifEven double

getIfEven f = (\x -> ifEven f x)

getIfXEven x = (\f -> ifEven f x)

ifEvenInc = getIfEven inc

ifEvenSquare = getIfEven square

ifEvenDouble = getIfEven double

getRequestUrl host apikey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?toke=" ++ apikey

genHostRequestBuilder host = (\apikey resource id -> getRequestUrl host apikey resource id)

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

genApiRequestBuilder' hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)

genUrlBuilder id = getRequestUrl "http://example.com" "1337hAsk3ll" "book" id

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

-- addressLetter' location name = addressLetter name location

flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

addressLetter' = flipBinaryArgs addressLetter

subtract2 x = flip (-) 2 x

binaryPartialApplication f x = (\y -> f x y)
