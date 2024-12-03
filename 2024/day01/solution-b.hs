-- Given two lists of numbers, eg.
-- [3, 4, 2, 1, 3, 3] and [4, 3, 5, 3, 9, 3]
-- Find how often each number in the left list appears
-- in the right list. These lists no longer require sorting.
-- The "similarity score" is the product of the left number
-- with how many times it appears in the right list.
-- eg. 3 appears 3 times, so 3 * 3 = a score of 9
--
-- We could probably do some optimization by searching once per number
-- and multiplying by appearances in the left list, eg. 3 * 3 * 3
-- but... eh

-- We need to parse the input, which comes as
-- left     right\n
-- left     right\n
parse :: String -> ([Int], [Int])
parse input = (left, right)
    where
        numbers = map (map read . words) $ lines input
        left = map head numbers
        right = map last numbers

-- count occurrences in a list
count :: Eq a => [a] -> a -> Int
count xs x = length . filter (==x) $ xs

-- Solve by finding each count and multiplying it
solve :: ([Int], [Int]) -> Int
solve (left, right) = sum scores
    where
        scores = zipWith (*) left counts
        counts = map (count right) left

-- Get the problem input from getContents, parse and solve
main = do
    input <- getContents
    print $ solve $ parse input
