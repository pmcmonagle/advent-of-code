-- Solution A --
-- Given a list of strings
-- eg. "vJrwpWtwJgWrhcsFMMfFFhFp"
-- Split the string in half: "vJrwpWtwJgWr", "hcsFMMfFFhFp"
-- and find the character which is repeated in both strings (eg. 'p')
-- Repeat for all lines, convert each character to an integer (eg. 'p' -> 16)
-- and sum the result

-- True if the list contains n
-- False if it does not
contains _ [] = False
contains n (x:xs)
    | x == n    = True
    | otherwise = contains n xs

-- Given a string, split it in half and find the first character
-- to be present in both halves
findRepeat :: String -> Char
findRepeat s =
    let len = div (length s) 2
        l = take len s
        r = drop len s
    in head [x | x <- l, contains x r]

-- Given a Char, determine its integer value
-- a-z -> 1-26, A-Z -> 27->52
toCharCode :: Char -> Integer
toCharCode c
    | c' > 96   = c' - 96  -- lowercase
    | otherwise = c' - 38  -- uppercase
    where c' = toInteger $ fromEnum c

solution :: String -> Integer
solution s = sum . map (toCharCode . findRepeat) $ lines s

-- Solution B --
-- Give a list of strings, organize the list into groups of 3
-- Find the common character between each group of 3 and sum the result

-- Given three strings, find the first char that is present in all 3
findRepeat' :: [String] -> Char
findRepeat' (a:b:c:_) = head [x | x <- a, contains x b && contains x c]

-- Given a list of strings, group them into sets of 3
group :: [String] -> [[String]]
group [] = []
group (a:b:c:xs) = [[a, b, c]] ++ group xs

solution' :: String -> Integer
solution' s = sum . map (toCharCode . findRepeat') . group $ lines s

-- Main --
main = do
    input <- getContents
    print $ solution' input
