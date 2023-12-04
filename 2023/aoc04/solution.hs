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

main = do
    input <- getContents
    print $ solution_a input
