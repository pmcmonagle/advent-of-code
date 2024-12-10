import Data.List
import Data.List.Split
import Data.Maybe

-- Given a list of resul in the form of a|b
-- And a list of lists of numbers eg. [1,2,3,4,5]
-- filter the lists of numbers by following
-- the rule: the first instance of a must preceed b
--
-- For Part 2, we drop the correct lists, and sort
-- the incorrect lists until they are correct.
--
-- Then, find the middle number of each valid list
-- and return the same.
--
-- eg. for rules 1|2 and 3|4
-- [1,2,3,4,5] is valid and [2,1,3,4,5] is not
-- We can drop the first list, and sort the second
-- (perhaps by swapping positions for incorrect
-- entries until all rules match?). The sorted
-- second list is [1,2,3,4,5], and its middle
-- number is 3.

-- Parse the input, into a list of tuples
-- for the rules eg. [(1,2), (3,4)] and a
-- list of lists of numbers for the data
-- eg. [[1,2,3], [4,5,6]]
parse :: String -> ([(Int, Int)], [[Int]])
parse input = (rules, nums)
    where
        (rawRules, rawNums) = break (== "") $ lines input
        parseRule (l, r) = (read l, read $ tail r)
        rules = map (parseRule . (break (== '|'))) rawRules
        nums = map ((map read) . (splitOn ",")) $ tail rawNums

-- We'll need this in a few places now
defies :: [Int] -> (Int, Int) -> Bool
defies xs (a,b) = isSubsequenceOf [b,a] xs

-- A list is valid if all rules are satisfied
isInvalid :: [(Int, Int)] -> [Int] -> Bool
isInvalid rules xs = any (defies xs) rules

-- Replace the element at an index with a value
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = (take i xs) ++ [x] ++ (drop (i+1) xs)

-- Swap the elements at two indices
swap :: Int -> Int -> [Int] -> [Int]
swap a b xs =
    let x = xs !! a
        y = xs !! b
    in replaceAtIndex b x $ replaceAtIndex a y xs

-- Apply a rule to a set by swapping the elements
apply :: [Int] -> (Int, Int) -> [Int]
apply xs (a,b)
    | isNothing indexA = xs
    | isNothing indexB = xs
    | (Just indexA) < (Just indexB) = xs
    | otherwise = swap (fromJust indexA) (fromJust indexB) xs
    where
        indexA = elemIndex a xs
        indexB = elemIndex b xs

-- Apply all rules to a set
applyAll :: [(Int, Int)] -> [Int] -> [Int]
applyAll [] xs = xs
applyAll (r:rs) xs = applyAll rs $ apply xs r

-- Correct a set by applying the rules to it until it is correct
correct :: [(Int, Int)] -> [Int] -> [Int]
correct rules xs
    | not $ isInvalid rules xs = xs
    | otherwise = correct rules $ applyAll rules xs

-- All of the above feels a bit messy, but it worked out okay
-- To solve, filter out the correct lists. Then, apply corrections
-- to all of the remaining lists until they are correct. Finally,
-- get the midpoints of each and return their sum
solve (rules, xss) =
    let filtered = filter (isInvalid rules) xss
        corrected = map (correct rules) filtered
        getMids xs = head $ (drop $ length xs `div` 2) xs
    in sum $ map getMids corrected

main = do
    input <- getContents
    print $ solve $ parse input
