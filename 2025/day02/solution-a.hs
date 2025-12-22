import Data.List.Split

-- Given a list of number ranges, eg. 10-20,30-40
-- Find all numbers within the range, inclusive, where
-- the first half of the digits is equal to the second half.
--
-- For example, in 10-20, 11 is the same number repeated 1 and 1
-- The range 446443-446449 contains the number 446446, 446=446
--
-- Find all such numbers within each range, and provide the sum

type Range = (Int, Int)

-- toRange "11-22" -> (11, 22)
toRange :: String -> Range
toRange s =
    let xs = splitOn "-" s
    in (read $ head xs, read $ last xs)

parse :: String -> [Range]
parse input =
    let xs = splitOn "," input
    in map toRange xs

isDouble :: Int -> Bool
isDouble n
    | l `rem` 2 == 1       = False
    | take h s == drop h s = True
    | otherwise            = False
    where
        s = show n
        l = length s
        h = div l 2

-- doublesInRange (11-22) -> [11,22]
doublesInRange :: Range -> [Int]
doublesInRange (min, max) = [x | x <- [min..max], isDouble x]

solve :: [Range] -> Int
solve rs = sum $ rs >>= doublesInRange

main = do
    input <- getContents
    print $ solve $ parse input
