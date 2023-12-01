-- Given a list of integers, eg.
-- 1, 2, 3, 2, 4, 1, 5
-- Get the sum of each group of three
-- -> 6, 7, 9, 7, 10
average :: [Int] -> [Int]
average []     = []
average [_]    = []
average [_, _] = []
average xs = (sum $ take 3 xs) : (average $ tail xs)

-- Given a list of integers, eg.
-- 1, 2, 3, 2, 4, 1, 5
-- Count the number of times that the next integer was greater than the previous
count :: [Int] -> Int -> Int
count [] a  = a
count [x] a = a
count (x:y:xs) a
    | y > x     = count (y:xs) (a+1)
    | otherwise = count (y:xs) a

parse :: String -> [Int]
parse s = map read $ lines s 

-- Problem 1
solution_a input = count (parse input) 0

-- Problem 2
solution_b input = count (average $ parse input) 0

main = do
    input <- getContents
    print $ solution_a input
    print $ solution_b input
