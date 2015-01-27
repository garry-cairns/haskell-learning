module Golf where

    import Data.List

    -- Exercise 1
    -- Take a list xs and an integer n, and return a list of every nth element of xs
    everyNth :: [a] -> Int -> [a]
    everyNth xs n = [snd x | x <- (zip [1..] xs), fst x `mod` n == 0]

    skips :: [a] -> [[a]]
    skips xs = map (everyNth xs) [1..length xs]

    -- Exercise 2
    -- Take a list of integers and produce a list of local maxima
    triples :: [a] -> [[a]]
    triples (x:xs)
      | length (x:xs) < 3 = []
      | otherwise = (x:take 2 xs) : triples xs

    isMiddleMax :: (Ord a) => [a] -> Bool
    isMiddleMax (x:y:z:[]) = x < y && y > z

    localMaxima :: [Integer] -> [Integer]
    localMaxima xs = map maximum . filter isMiddleMax $ triples xs

    -- Exercise 3
    -- Take a list of integers 0-9 and return a histogram
    groupSort :: (Eq a, Ord a) => [a] -> [[a]]
    groupSort xs = group $ sort xs

    countInstance :: (Eq a, Ord a) => [a] -> (a, Int)
    countInstance xs = (head xs, length xs)

    countInstances :: (Eq a, Ord a) => [a] -> [(a, Int)]
    countInstances xs = map (countInstance) $ groupSort xs

    maxInstances :: [Int] -> Int
    maxInstances xs = maximum $ map (snd) $ countInstances xs

    spaceOrStar :: [Int] -> Int-> Char
    spaceOrStar xs y
      | y `elem` xs = '*'
      | otherwise = ' '

    getData :: (Eq a, Ord a) => [a] -> Int -> [a]
    getData xs n = [fst x | x <- countInstances xs, snd x >= n]

    histRow :: [Int] -> String
    histRow xs = map (spaceOrStar xs) [0..9]

    buildHist :: [Int] -> String
    buildHist xs = intercalate "\n" . map (histRow) $ map (getData xs) . reverse $ [1..maxInstances xs]

    histogram :: [Int] -> String
    histogram xs = buildHist xs ++ "\n==========\n0123456789\n"

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
        putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
