import Data.Char
import Data.List

-- Split a list on some predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)

-- Problem A
-- Given a list of strings in the form of:
--   Game 1: blue 2, green 3; red 14; etc.
-- Sum the ID's of games which do not have more than 12 red, 13 green, 13 blue

-- Let's define a Game by its ID, followed by a list of (r,g,b) tuples, each representing a round
type Round = (Int, Int, Int)
type Game  = (Int, [Round])

-- PARSING --
parseGrab :: String -> Round
parseGrab xs =
    let input  = dropWhile (==' ') xs
        count  = takeWhile (/=' ') input
        colour = drop 1 $ dropWhile (/=' ') input
    in case colour of
        "red" -> (read count, 0, 0)
        "green" -> (0, read count, 0)
        "blue" -> (0, 0, read count)
        _ -> (0, 0, 0)

parseTotals :: [Round] -> Round -> Round
parseTotals [] x = x
parseTotals ((a, b, c):xs) (x, y, z) = parseTotals xs (a+x, b+y, c+z)

parseRound :: String -> Round
parseRound xs =
    let input = dropWhile (==' ') xs
        grabs = map parseGrab $ splitOn (==',') input
    in parseTotals grabs (0, 0, 0)

parseGame :: String -> Game
parseGame xs =
    let id = read . takeWhile (/=':') $ drop 5 xs
        rounds = map parseRound $ splitOn (==';') (drop 1 $ dropWhile (/=':') xs)
    in (id, rounds)

parse :: String -> [Game]
parse input = map parseGame $ lines input

-- Predicates to disallow certain conditions
isValidRound :: Round -> Bool
isValidRound (r, g, b)
    | r > 12 = False
    | g > 13 = False
    | b > 14 = False
    | otherwise = True

isValidGame :: Game -> Bool
isValidGame (_, rounds) = not (elem False $ map isValidRound rounds)

-- Solution A
-- Sum the IDs of all valid games given the above predicates
solution_a :: [Game] -> Int
solution_a gs =
    let validGames = filter isValidGame gs
        ids = map fst validGames
    in sum ids

getMaximum :: Game -> Round
getMaximum (_, rounds) = accumulate rounds (0, 0, 0)
    where
        accumulate [] result = result
        accumulate ((r, g, b): rest) (x, y, z) = accumulate rest (max r x, max g y, max b z)

calculatePower :: Round -> Int
calculatePower (x, y, z) = x * y * z

-- Solution B
-- Sum the POWERs (r * g * b) of the maximum r, g, b values for each game
solution_b :: [Game] -> Int
solution_b gs =
    let maximums = map getMaximum gs
        powers   = map calculatePower maximums
    in sum powers

-- Main
main = do
    input <- getContents
    print $ solution_a (parse input)
    print $ solution_b (parse input)
