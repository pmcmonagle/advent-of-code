import Data.List

-- Given a matrix... ugghhhhh
-- Okay, given a matrix of chars, look for all
-- Instances of the character 'A' with two 'M's and two 'S's
-- at it's corners, forming the word "MAS" twice
-- M . M     M . S      M . S
-- . A .  or . A .  not . A .
-- S . S     M . S      S . M
--
-- Almost nothing I did in Part 1 will help me here.
-- Instead I think I need to get a list of indices for 'A'
-- eg. [(1,1), (4,6), etc.]
-- and filter them based off of their neighbours

-- Retrieve the value at an (x,y) point
valueAt :: (Int, Int) -> [[a]] -> a
valueAt (x,y) xss = head $ drop x $ head $ drop y xss

-- First let's get some (x,y) points where 'A's live
-- Ignore the outer edges since they can't be part of the solution
indices :: Eq a => a -> [[a]] -> [(Int, Int)]
indices n hs = foldr searchRow [] [1..(length hs)-2]
    where
        searchRow y acc' = foldr (\x acc -> if valueAt (x,y) hs == n then (x,y):acc else acc) acc' [1..(length $ head hs)-2]

-- Now let's get some corners around those indices
-- Group them into diagonals to make it easier to compare later
-- eg. ("MS", "SM") for a match and ("XM", "SS") for no match
corners :: [String] -> [(Int, Int)] -> [(String, String)]
corners hs = map corner
    where corner (x,y) = ((valueAt (x-1,y-1) hs):(valueAt (x+1,y+1) hs):[], (valueAt (x+1,y-1) hs):(valueAt (x-1,y+1) hs):[])

-- Get a list of indices of 'A'
-- Get a list of diagonal corners from that
-- Filter by diagonal corners being MS or SM 
-- I think this was easier than Part 1?
solve :: [String] -> Int
solve input = length $ filter valid cs
    where
        is = indices 'A' input
        cs = corners input is
        valid (a, b) = (a == "MS" || a == "SM") && (b == "MS" || b == "SM")

-- Strings are already lists so we just have
-- to separate the lines
parse :: String -> [[Char]]
parse input = lines input

main = do
    input <- getContents
    print $ solve $ parse input
