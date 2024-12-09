import Data.List (isInfixOf, isSubsequenceOf)
import Data.List.Split

-- Given a list of resul in the form of a|b
-- And a list of lists of numbers eg. [1,2,3,4,5]
-- filter the lists of numbers by following
-- the rule: the first instance of a must preceed b
--
-- Then, find the middle number of each valid list
-- and return the same.
--
-- eg. for rules 1|2 and 3|4
-- [1,2,3,4,5] is valid and [2,1,3,4,5] is not
-- The middle number from the valid list is 3

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

-- A list is valid if all rules are satisfied
isValid :: [(Int, Int)] -> [Int] -> Bool
isValid rules xs = all obeys rules
    where obeys (a,b) = not $ [b,a] `isSubsequenceOf` xs

solve (rules, xss) =
    let filtered = filter (isValid rules) xss
        getMids xs = head $ (drop $ length xs `div` 2) xs
    in sum $ map getMids filtered

main = do
    input <- getContents
    print $ solve $ parse input
