import Data.Char
import Data.List

-- eg. ('32T3K', 765)
type Hand = (String, Int)

parse :: String -> [Hand]
parse input = map parseLine $ lines input
    where parseLine l = let (h, t) = break (==' ') l in (h, read t)

letterScore :: Char -> Int
letterScore c
    | c == 'T'  = 10
    | c == 'J'  = 11
    | c == 'Q'  = 12
    | c == 'K'  = 13
    | c == 'A'  = 14
    | otherwise = digitToInt c

-- Convert the string into a base score first
-- The maximum score would be AAAAA -> 1414141414
baseScore :: String -> Int
baseScore (a:b:c:d:e:_) =
    (letterScore a * 100000000)
    + (letterScore b * 1000000)
    + (letterScore c * 10000)
    + (letterScore d * 100)
    + (letterScore e)

typeScore :: String -> Int
typeScore hand
    | length g == 5 = 6
    | length g == 4 = 5
    | length g == 3 = if (length $ head gs) == 2 then 4 else 3
    | length g == 2 = if (length $ head gs) == 2 then 2 else 1
    | otherwise = 0
    where (g:gs) = reverse $ sortOn length ((group . sort) hand)

score :: Hand -> Int
score (h, _) = (typeScore h) * 10000000000 + (baseScore h)

solution_a :: String -> Int
solution_a input =
    let sorted = sortOn score (parse input)
        scores = map snd sorted
    in sum $ zipWith (*) [1..] scores

main = do
    input <- getContents
    print $ solution_a input
