-- Solution A --
-- Given a string, eg.
-- mjqjpqmgbljsphdztnvjfqwrcgsmlb
--
-- Determine the earliest index at which the preceding 4 characters are all
-- different from one another
--
-- For the above string, this value is 7, after skipping past mjq to find jpqm

-- True the list contains the needle
-- False otherwise
contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains n (x:xs)
    | n == x    = True
    | otherwise = contains n xs

-- True if no value in a list is repeated
-- False otherwise
isUnique :: (Eq a) => [a] -> Bool
isUnique []  = True
isUnique [x] = True
isUnique (x:xs)
    | contains x xs = False
    | otherwise     = isUnique xs

-- Custom break that breaks at n unique values in a list
break' :: (Eq a) => Int -> [a] -> ([a], [a])
break' n l@(x:xs)
    | length l <= n       = (l, [])
    | isUnique $ take n l = (take n l, drop n l)
    | otherwise           = (x:hs, ts)
    where (hs, ts) = break' n xs

solution :: String -> Int
solution s =
    let (l, r) = break' 4 s
    in length l

-- Solution B --
-- Same as above but with 14 distinct characters rather than 4

solution' :: String -> Int
solution' s =
    let (l, r) = break' 14 s
    in length l


-- Main --
main = do
    input <- getContents
    print $ solution' input
