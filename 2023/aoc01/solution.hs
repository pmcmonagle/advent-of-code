-- Given a text file
-- Output the text file!

-- Sort a list, smallest to biggest!
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smaller = quicksort [a | a <- xs, a <= x]
      bigger  = quicksort [a | a <- xs, a > x]
  in smaller ++ [x] ++ bigger

parse cs = words cs

-- Main
main = do
    input <- getContents
    mapM_ print $ parse input
