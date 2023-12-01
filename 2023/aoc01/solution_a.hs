import Data.Char

-- Given a list of alphanumeric strings
-- eg. abc123xyz456
-- Grab the first and last digit eg. 1,6
-- Combine it into a number 1,6 -> 16
-- And sum the result from each line

-- Quicksort, just to remind myself how to use Haskell
-- Sort a list, smallest to biggest!
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smaller = quicksort [a | a <- xs, a <= x]
      bigger  = quicksort [a | a <- xs, a > x]
  in smaller ++ [x] ++ bigger

firstLast :: [Int] -> Int
firstLast is = read $ (show $ head is) ++ (show $ last is)

getDigits :: [Char] -> [Char]
getDigits ds = filter isDigit ds

parse :: String -> [[Int]]
parse cs = map (map digitToInt . getDigits) (lines cs)

-- Main
main = do
    input <- getContents
    print . sum $ (map firstLast) (parse input)
