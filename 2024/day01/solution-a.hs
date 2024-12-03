-- Given two lists of numbers, eg.
-- [3, 4, 2, 1, 3, 3] and [4, 3, 5, 3, 9, 3]
-- 1. Sort the lists from smallest to largest
-- 2. find the difference between the pairs at each index
-- 3. Sum the resulting list of differences

-- Let's implement quick-sort again, because we need it,
-- and because it's a good way to remember how to write haskell
sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:xs) = (sort smaller) ++ [x] ++ (sort larger)
    where
        smaller = filter (<= x) xs
        larger = filter (> x) xs

-- We also need to parse the input, which comes as
-- left     right\n
-- left     right\n
-- We may as well sort the result while we're at it
parse :: String -> ([Int], [Int])
parse input = (sort left, sort right)
    where
        numbers = map (map read . words) $ lines input
        left = map head numbers
        right = map last numbers

-- Finally, solve by finding the differences and returning the sum
solve :: ([Int], [Int]) -> Int
solve (left, right) = sum differences
    where
        differences = map abs $ zipWith (-) left right

-- Get the problem input from getContents, parse and solve
main = do
    input <- getContents
    print $ solve $ parse input
