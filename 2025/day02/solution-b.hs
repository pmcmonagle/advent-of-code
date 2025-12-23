import Data.List.Split

-- Solution B is identical to Solution A except that
-- we need to search for repeating patterns of any length
-- eg. 123123123, 446446, 11, 1001100110011001 are patterns
--     10, 44644, 123123132 are not

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

-- Test chunks of length n, subtracting 1 until
-- we find a matching pattern or hit 0
testPattern :: Int -> String -> Bool
testPattern 0 _ = False
testPattern n s
  | all (== c) cs = True
  | otherwise     = testPattern (n-1) s
  where (c:cs) = chunksOf n s

isPattern :: Int -> Bool
isPattern x =
  let
    s = show x
    l = length s
    h = l `div` 2
  in testPattern h s

-- patternsInRange (11-22) -> [11,22]
patternsInRange :: Range -> [Int]
patternsInRange (min, max) = [x | x <- [min..max], isPattern x]

solve :: [Range] -> Int
solve rs = sum $ rs >>= patternsInRange

main = do
    input <- getContents
    print $ solve $ parse input
