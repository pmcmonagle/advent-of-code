import Data.Char

-- (Card ID, Winning Nos, Your Nos)
type Card = (Int, [Int], [Int])

-- Split a list on some predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)

parseCard :: String -> Card
parseCard s =
    let (hs:ts:_) = splitOn (==':') s
        (ws:ys:_) = splitOn (=='|') ts
        id = read $ filter isDigit hs
        wins = map read $ words ws
        yous = map read $ words ys
    in (id, wins, yous)

score :: Card -> Int
score (_, ws, ys) =
    let valid n = elem n ws
        count   = length $ filter valid ys
    in case count of
        0 -> 0
        _ -> 2 ^ (count - 1)

solution_a :: String -> Int
solution_a input = sum $ map (score . parseCard) (lines input)

-- Like exploding die, each "win" copies a card below it, which can also explode
explode :: [Card] -> [Card] -> [Card]
explode cards [] = []
explode cards (c@(id, ws, ys):cs) =
    let valid n = elem n ws
        count   = length $ filter valid ys
        remains = drop id cards
    in case count of
        0 -> c : (explode cards cs)
        _ -> c : (explode cards cs) ++ (explode cards $ take count remains)

solution_b :: String -> Int
solution_b input =
    let cards = map parseCard (lines input)
    in length $ explode cards cards

main = do
    input <- getContents
    print $ solution_a input
    print $ solution_b input
