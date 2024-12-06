import Data.List

-- Given a matrix... ugghhhhh
-- Okay, given a matrix of chars, look for all
-- instances of the string XMAS, forwards,
-- backwards, up, down, diagonal... all
--
-- ..X...
-- .SAMX.
-- .A..A.
-- XMAS.S
-- .X....
--
-- The above example has four instances
--
-- Maybe we can just rotate the matrix and do
-- some simple string searches. The only tricky
-- bit with that is diagonals, but I think we
-- can just push each element, in order, to the
-- front of a list (or the same in reverse)
--
--           1
-- 1 2 3    4 2
-- 4 5 6   7 5 3
-- 7 8 9    8 6
--           9
-- [[1], [2], [3]] -> [[1], [4, 2], [5, 3], [6]] -> etc

-- Our little test array from above, because it's
-- easier to read than the actual test input
test = [
    "..X...",
    ".SAMX.",
    ".A..A.",
    "XMAS.S",
    ".X...."]

-- Append an element to the end of a sublist
appendAt :: Int -> a -> [[a]] -> [[a]]
appendAt i x xs = (take i xs) ++ [(xs !! i) ++ [x]] ++ (drop (i+1) xs)

-- rotate a list, 90 degrees clockwise, n times!
rotate90 :: Int -> [[a]] -> [[a]]
rotate90 0 xs = xs
rotate90 n xs = rotate90 (n-1) $ map reverse . transpose $ xs

-- rotate a list, 45 degrees clockwise, once!
-- This took me absolutely AGES because I kept trying to find
-- a "haskell" way to do it without array indices.
rotate45 :: [[a]] -> [[a]]
rotate45 xss = foldr apply empty indexed
    where
        empty = take (length xss * 3 - 1) $ repeat []
        indexed = zip [0..] (map (zip [0..]) xss)
        apply (i, row) acc' = foldr (\(j, val) acc -> appendAt (i+j) val acc) acc' row 

-- Okay now we need to count occurrances of a substring
count :: String -> String -> Int
count n hs = foldr (\i acc -> if n `isPrefixOf` (drop i hs) then acc + 1 else acc) 0 [0..(length hs)]

solve :: [String] -> Int
solve input = sum [a, b, c, d]
    where
        getSums xs = (sum $ map (count "XMAS") xs) + (sum $ map (count "SAMX") xs)
        a = getSums input
        b = getSums $ rotate90 1 input
        c = getSums $ rotate45 input
        d = getSums $ rotate45 $ rotate90 1 input

-- Strings are already lists so we just have
-- to separate the lines
parse :: String -> [[Char]]
parse input = lines input

main = do
    input <- getContents
    print $ solve $ parse input
