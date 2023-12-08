import qualified Data.Map as Map

parseMapEntry :: String -> (String, (String, String))
parseMapEntry s =
    let k = take 3 s
        l = take 3 $ drop 7 s
        r = take 3 $ drop 12 s
    in (k, (l, r))

-- navigate :: Map -> String -> String
navigate mp loc [] = []
navigate mp loc (p:ps)
    | loc == "ZZZ" = []
    | otherwise    = p:(navigate mp next ps)
    where next = if p == 'L' then fst (mp Map.! loc) else snd (mp Map.! loc)

solution_a input =
    let ls = lines input
        is = cycle $ head ls
        mp = Map.fromList $ map parseMapEntry (drop 2 ls)
    in navigate mp "AAA" is

-- Solution B requires navigating a list of locations
-- This takes *forever* and I bet I need to find a fixed point for each start location
-- and calculate when they all overlap
navigate' mp locs [] = []
navigate' mp locs (p:ps)
    | all ((=='Z') . head) locs = []
    | otherwise                 = p:(navigate' mp nexts ps)
    where nexts = if p == 'L' then map (fst . (mp Map.!)) locs else map (snd . (mp Map.!)) locs

solution_b input =
    let ls = lines input
        is = cycle $ head ls
        xs = map parseMapEntry (drop 2 ls)
        ss = filter ((=='A') . last) (map fst xs)
        mp = Map.fromList xs
    in navigate' mp ss is

main = do
    input <- getContents
    print . length $ solution_a input
    print . length $ solution_b input
