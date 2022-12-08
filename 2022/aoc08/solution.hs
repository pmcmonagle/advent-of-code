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
        x = head ts
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

-- Main --
main = do
    input <- getContents
    print . solution $ map (map digitToInt) (lines input)
