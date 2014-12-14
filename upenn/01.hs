-- Double the value of every second digit beginning from the right.  That is, the last digit is unchanged; the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on. For example, [1,3,8,6] becomes [2,3,16,6].
--
-- Add the digits of the doubled values and the undoubled digits from the original number. For example, [2,3,16,6] becomes 2+3+1+6+6 = 18.
--
-- Calculate the remainder when the sum is divided by 10. For the above example, the remainder would be 8. If the result equals 0, then the number is valid.
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = [read [x] :: Integer | x <- show n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x : []) = [x]
doubleEveryOther (x : xs)
  | length xs `mod` 2 == 0 = (x : doubleEveryOther xs)
  | otherwise = (x*2 : doubleEveryOther xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:xs) = sum (toDigits x ++ [sumDigits xs])

validate :: Integer -> Bool
validate n
  | x `mod` 10 == 0 = True
  | otherwise = False
  where x = sumDigits . doubleEveryOther $ toDigits n

main :: IO()
main = do
    print $ toDigits 1234 == [1, 2, 3, 4]
    print $ toDigitsRev 1234 == [4, 3, 2, 1]
    print $ toDigits 0 == []
    print $ toDigits (-17) == []
    print $ doubleEveryOther [8, 7, 6, 5] == [16, 7, 12, 5]
    print $ doubleEveryOther [1, 2, 3] == [1, 4, 3]
    print $ sumDigits [16, 7, 12, 5] == 22
    print $ validate 4012888888881881 == True
    print $ validate 4012888888881882 == False
