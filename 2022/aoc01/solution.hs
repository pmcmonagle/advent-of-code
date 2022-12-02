-- Given a text file in form of
-- 10
-- 20
-- 
-- 30
-- 10
--
-- etc 
-- Determine the unbroken sequence with the highest count

-- Sort a list, smallest to biggest!
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smaller = quicksort [a | a <- xs, a <= x]
      bigger  = quicksort [a | a <- xs, a > x]
  in smaller ++ [x] ++ bigger

-- Split a list of string using an empty string as a boundary
split :: [String] -> [[String]]
split s = case dropWhile (=="") s of
    [] -> []
    s' -> w : split s''
        where (w, s'') = break (=="") s'

parse :: String -> [[Integer]]
parse s = map (map read) . split $ lines s

main = do
    input <- getContents
    print . sum . take 3 . reverse . quicksort . map sum $ parse input
