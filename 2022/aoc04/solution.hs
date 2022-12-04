-- From Data.List.Split
-- Split a list on some predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)

-- Solution A --
-- Given a list of number pairs
-- eg.
-- 2-4,3-6
--
-- Determine the number of pairs where
-- one pair fully contains the other
-- eg.
-- 3-7,2-8 <-- 3-7 is fully contained by 2-8

-- Given a pair of ranges eg. [[2,3], [4,5]]
-- Determine if the ranges fully overlap
pairsOverlap :: [[Integer]] -> Bool
pairsOverlap (l:r:_)
    | head l <= head r && last l >= last r = True
    | head r <= head l && last r >= last l = True
    | otherwise = False

solution :: String -> Int
solution s =
    let pairs = map (map $ splitOn (=='-')) . map (splitOn (==',')) $ lines s
    in length [x | x <- pairs, pairsOverlap $ map (map read) x]

-- Solution B --

solution' :: String -> Integer
solution' s = 0

-- Main --
main = do
    input <- getContents
    print $ solution input
