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

main = do
    input <- getContents
    print . length $ solution_a input
