-- Exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0                             = []
  | n `div` 10 == 0 && n `mod` 10 == n = [n] -- a single decimal digit
  | otherwise                          = n `mod` 10 : toDigitsRev(n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse(toDigitsRev n)

-- Exercise 2

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft []         = []  -- do nothing to the empty list
doubleEveryOtherFromLeft (x:[])     = [x] -- do nothing to lists with a single element
doubleEveryOtherFromLeft (x:y:z)    = x : (y * 2) : doubleEveryOtherFromLeft z

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse(doubleEveryOtherFromLeft(reverse n))

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits n = sum(concat(map toDigits n))

-- Exercise 4

validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits n)) `mod` 10 == 0
