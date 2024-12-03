-- Given lists of numbers, eg.
-- 7, 6, 4, 2, 1
-- 1, 2, 7, 8, 9
-- etc.
--
-- Determine how many lists are "safe"
-- ie. all numbers are either incrementing or decrementing
--     numbers are different by at least 1 and at most 3
--
-- or "unsafe"
-- ie. there is a mix of decrementing and incrementing
--     or numbers differe by less than 1 or more than 3

-- We'll want to map each list to a list of differences
-- eg. [7, 6, 4, 2, 1] -> [1, 2, 2, 1] 
diffs :: [Int] -> [Int]
diffs [] = []
diffs [x] = []
diffs (x:xs) = [x - (head xs)] ++ (diffs xs)

-- Whether our difference is in the safe zone, eg.
-- between 1 and 3
justRight :: Int -> Bool
justRight x = abs x > 0 && abs x < 4

-- xnor [True, True] -> True
-- xnor [False, False] -> True
-- xnor [True, False] -> False
xnor :: [Bool] -> Bool
xnor xs = and xs || and (map not xs)

-- Make sure all differences are:
-- 1. The same sign (all + or all -)
-- 2. In the "safe zone" defined above
safe :: [Int] -> Bool
safe xs = allOkay && sameSign
    where
        differences = diffs xs
        allOkay = and $ map justRight differences
        sameSign = xnor $ map (> 0) differences

-- Parse our input into lists of numbers
parse :: String -> [[Int]]
parse input = map (map read . words) $ lines input

-- Solve by counting the number of safe lists
solve :: [[Int]] -> Int
solve input = length $ filter safe input

-- Get contents, parse the input and solve it
main = do
    input <- getContents
    print $ solve $ parse input
