import Data.List.Split
import Text.Regex.PCRE

-- Given some string; eg.
-- xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
--
-- Parse the string to look for instances of mul(x,y) where x and y are some integer
-- Multiply these integers together, and sum the results. Eg. from the above string
-- We can find mul(2,4), mul(5,5), mul(11,8), mul(8,5) to form
-- 2*4 + 5*5 + 11*8 + 8*5 = 161
--
-- This looks like a good use for regular expressions, but I have a feeling
-- that the rules for part b will no longer be a regular grammar

-- Solving in this case should just be multiplying the number pairs
-- and returning the sum
solve :: [(Int, Int)] -> Int
solve pairs = sum $ map product pairs
    where product (a,b) = a * b

-- Parsing is the meat of this problem
parse :: String -> [(Int, Int)]
parse input = map (toTuple . (map read) . toList) $ search input
    where
        toList s = splitOn "," s
        toTuple (x:xs) = (x, head xs)

-- Regex!
-- Sorting out the return types for this was a bigger pain than I expected
-- And then I had to switch to PCRE for the lookaheads / lookbehinds
-- Which requires libpcre3-dev...
search :: String -> [String]
search input = getAllTextMatches $ (input =~ regex :: AllTextMatches [] String)
    where regex = "(?<=mul\\()([0-9]+,[0-9]+)(?=\\))"

main = do
    input <- getContents
    print $ solve $ parse input
