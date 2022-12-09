import Data.List

-- Some utils to help us move around the grid,
-- one step at a time

type H = (Int, Int)
type T = (Int, Int)
type Rope = (H, T)
type Instruction = (Char, Int)

right :: Rope -> Rope
right ((hx, hy), (tx, ty))
    | tx == hx   = ((hx+1, hy), (tx, ty))
    | tx == hx+1 = ((hx+1, hy), (tx, ty))
    | otherwise  = ((hx+1, hy), (tx+1, hy))

left :: Rope -> Rope
left ((hx, hy), (tx, ty))
    | tx == hx   = ((hx-1, hy), (tx, ty))
    | tx == hx-1 = ((hx-1, hy), (tx, ty))
    | otherwise  = ((hx-1, hy), (tx-1, hy))

up :: Rope -> Rope
up ((hx, hy), (tx, ty))
    | ty == hy   = ((hx, hy+1), (tx, ty))
    | ty == hy+1 = ((hx, hy+1), (tx, ty))
    | otherwise  = ((hx, hy+1), (hx, ty+1))

down :: Rope -> Rope
down ((hx, hy), (tx, ty))
    | ty == hy   = ((hx, hy-1), (tx, ty))
    | ty == hy-1 = ((hx, hy-1), (tx, ty))
    | otherwise  = ((hx, hy-1), (hx, ty-1))

execute :: [Rope] -> [Instruction] -> [Rope]
execute rs [] = rs
execute (r:rs) (i:is) = case i of 
    ('L', 1) -> execute ((left r):r:rs) is
    ('L', n) -> execute ((left r):r:rs) (('L', n-1):is)
    ('R', 1) -> execute ((right r):r:rs) is
    ('R', n) -> execute ((right r):r:rs) (('R', n-1):is)
    ('U', 1) -> execute ((up r):r:rs) is
    ('U', n) -> execute ((up r):r:rs) (('U', n-1):is)
    ('D', 1) -> execute ((down r):r:rs) is
    ('D', n) -> execute ((down r):r:rs) (('D', n-1):is)
    (unk, n) -> execute (r:rs) is

-- Solution A --
-- Given a cartesian grid with two elements, H, T
-- Start both at (0,0)
-- Read a series of instructions in the form of
-- ('R', 4), ('U', 2) etc.
--
-- H follows these intructions exactly
-- T follows H such that it will match the row (for 'R', 'L') or collumn (for 'U', 'D')
-- of H, but will only move until it touches H
--
-- Execute the series of instructions, and determine the number of unique positions that
-- T is placed in

-- We want to transform our list of instructions in a list of tail positions
-- And then count the number of unique values
solution :: Rope -> [Instruction] -> Int
solution r is =
    let rs       = execute [r] is
        (hs, ts) = unzip rs
    in length $ nub ts

-- Solution B --

-- Main --
parseLine :: String -> (Char, Int)
parseLine s =
    let h = head s
        t = read . last $ words s
    in (h, t)

-- (H, T) both at (0, 0)
startPos = ((0, 0), (0, 0))

main = do
    input <- getContents
    print . (solution startPos) $ map parseLine (lines input)
