toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = map (\a -> read [a] :: Integer) (show n)
    -- alternative way
    -- | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

enumerate :: [Integer] -> [(Integer, Integer)]
enumerate [] = []
enumerate xs = zip [0..] xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x =
  let xs = enumerate x
  in [res | (index, value) <- xs , let res = value * (mod index 2 + 1)]


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) =  sumDigits xs + (sum . toDigits) x

validate :: Integer -> Bool
validate x = 
  let sum = (sumDigits . doubleEveryOther . toDigitsRev) x
  in (mod sum 10) == 0