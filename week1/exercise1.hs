toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = map (\a -> read [a] :: Integer) (show n)
    -- alternative way
    -- | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x =
  let xs = enumerate x
  in [res | (index, value) <- xs , let res = value * (mod index 2 + 1)]
  where 
    enumerate :: [Integer] -> [(Integer, Integer)]
    enumerate [] = []
    enumerate xs = zip [0..] xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap (\x -> toDigits x)

validate :: Integer -> Bool
validate = (0 ==) . (`mod` 10) . sum . doubleEveryOther . toDigitsRev