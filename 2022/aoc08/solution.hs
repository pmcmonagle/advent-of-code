import Data.Char
import Data.List

-- Solution A --
-- Given a grid of integers
-- eg
-- 30373
-- 25512
-- 65332
-- 33549
-- 35390
--
-- Determine how many are the highest number between
-- itself and at least one edge of the grid
-- eg. in 65-3-32 the centre 3 is lower than (or equal to) its neighbours
-- in both directions - blocked by a 5 one way and another 3 the other.
-- Looking at the same 3 vertically, 35-3-53 it is blocked by 5s as well

-- Determine if the value at position n is larger than all elements before,
-- or all elements after it
isLargest :: Int -> [Int] -> Bool
isLargest n xs =
    let (hs, ts) = splitAt n xs
        x        = head ts
    in find (>=x) hs == Nothing || find (>=x) (tail ts) == Nothing

solution :: [[Int]] -> Int
solution xss =
    let w   = length (head xss)
        h   = length xss
        yss = transpose xss
    in length [(x,y) | x <- [0..(w-1)],
                y <- [0..(h-1)],
                let (_,xs) = splitAt y xss
                    (_,ys) = splitAt x yss
                in isLargest x (head xs) || isLargest y (head ys)]

-- Solution B --
-- Similar to the above, except that now we are determining how many numbers
-- are smaller before we reach one that is higher or equal. Each grid point should
-- be mapped to four score (up, down, left, right).
-- For example 25-5-12 has 1 to the left (5) and 2 to the right (1,2)
-- The same number vertically, 3-5-353 has 1 up (3) and 2 down (3,5)
-- 
-- We need to score these by multiplying the four values (l*r*u*d) and find the largest score

-- Determine the number of values that are smaller than the value at position n
-- on either side of the list, until we reach a value that is equal to or larger
countSmaller :: Int -> [Int] -> [Int]
countSmaller n xs =
    let (hs, ts)  = splitAt n xs
        x         = head ts
        (ls, ls') = break (>=x) (reverse hs)
        (rs, rs') = break (>=x) (tail ts)
        l         = if ls' == [] then [] else [head ls']
        r         = if rs' == [] then [] else [head rs']
    in [length (ls++l), length (rs++r)]

-- Count the number of smaller neighbours and multiply them together
getScore :: [[Int]] -> (Int, Int) -> Int
getScore xss (x, y) =
    let yss    = transpose xss
        (_,xs) = splitAt y xss
        (_,ys) = splitAt x yss
    in product $ (countSmaller x $ head xs) ++ (countSmaller y $ head ys)

solution' :: [[Int]] -> [Int]
solution' xss =
    let w   = length (head xss)
        h   = length xss
        coords = [(x,y) | x <- [0..(w-1)],
                          y <- [0..(h-1)]]
        in map (getScore xss) coords

-- Main --
main = do
    input <- getContents
    print . maximum . solution' $ map (map digitToInt) (lines input)
