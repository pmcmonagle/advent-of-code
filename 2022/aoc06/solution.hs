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

-- Custom break that will split a list on
-- four items in a row being unique
break' :: (Eq a) => [a] -> ([a], [a])
break' l@[a,b,c,d] = (l, [])
break' l@(a:b:c:d:xs)
    | isUnique [a,b,c,d] = ([a,b,c,d], xs)
    | otherwise          = (a:hs, ts)
    where (hs, ts) = break' $ tail l

-- solution :: String -> Integer
solution s =
    let (l, r) = break' s
    in length l

-- Solution B --

-- Main --
main = do
    input <- getContents
    print $ solution input
