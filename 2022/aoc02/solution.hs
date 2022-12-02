-- SOLUTION A --
-- Given input in the form of
-- A Y
-- B X
-- C Z
--
-- Such that
-- A = 1, B = 2, C = 3
-- X = 1, Y = 2, Z = 3
-- and 
-- WIN = 6, DRAW = 3, LOSS = 0
-- where 1 < 2, 2 < 3, 3 < 1

-- Parse a single line into a tuple (left, right)
parse' :: String -> (Integer, Integer)
parse' ls = (convert $ head ls, convert $ last ls)
    where
        convert 'A' = 1
        convert 'B' = 2
        convert 'C' = 3
        convert 'X' = 1
        convert 'Y' = 2
        convert 'Z' = 3

-- Play a game of rock-paper-scissors
play' :: Integer -> Integer -> Integer
play' l r
    | l == r           = 3 -- Draw
    | l == 3 && r == 1 = 6 -- Win
    | l == 1 && r == 3 = 0 -- Loss
    | l < r            = 6 -- Win
    | otherwise        = 0 -- Loss

-- Score a tuple based on the rules describe above
score' :: (Integer, Integer) -> Integer
score' (l, r) = (play' l r) + r

solution' :: String -> Integer
solution' i = sum . map (score' . parse') $ lines i

-- SOLUTION B --
-- Given an input in the form of
-- A Y
-- B X
-- C Z

parse'' :: String -> Integer
parse'' (l:_:r:_)
    | l == 'A' && r == 'X' = 0 + 3 -- Lose, Scissors
    | l == 'A' && r == 'Y' = 3 + 1 -- Draw, Rock
    | l == 'A' && r == 'Z' = 6 + 2 -- Win,  Paper
    | l == 'B' && r == 'X' = 0 + 1 -- Lose, Rock
    | l == 'B' && r == 'Y' = 3 + 2 -- Draw, Paper
    | l == 'B' && r == 'Z' = 6 + 3 -- Win,  Scissors
    | l == 'C' && r == 'X' = 0 + 2 -- Lose, Paper
    | l == 'C' && r == 'Y' = 3 + 3 -- Draw, Scissors
    | l == 'C' && r == 'Z' = 6 + 1 -- Win,  Rock
    | otherwise            = 0

solution'' :: String -> Integer
solution'' i = sum . map parse'' $ lines i

main = do
    input <- getContents
    print $ solution'' input
