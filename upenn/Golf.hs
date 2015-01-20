module Golf where

    import Data.List

    everyNth :: [a] -> Int -> [a]
    everyNth xs n = [snd x | x <- (zip [1..] xs), fst x `mod` n == 0]

    skips :: [a] -> [[a]]
    skips xs = map (everyNth xs) [1..length xs]

    triples :: [a] -> [[a]]
    triples (x:xs)
      | length (x:xs) < 3 = []
      | otherwise = (x:take 2 xs) : triples xs

    isMiddleMax :: Ord a => [a] -> Bool
    isMiddleMax (x:y:z:[]) = x < y && y > z

    localMaxima :: [Integer] -> [Integer]
    localMaxima xs = map maximum . filter isMiddleMax $ triples xs

    histogram :: [Integer] -> String

    main :: IO()
    main = do
        print $ skips "ABCD"
        print $ skips "hello!"
        print $ skips [1]
        print $ skips [True, False]
        print $ skips ([] :: [Int])
        print $ localMaxima [2,9,5,6,1]
        print $ localMaxima [2,3,4,1,5]
        print $ localMaxima [1,2,3,4,5]
