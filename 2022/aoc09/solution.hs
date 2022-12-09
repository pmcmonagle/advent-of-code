import Data.List

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

-- Some utils to help us move around the grid,
-- one step at a time

type Knot = (Int, Int)
type Rope = (Knot, Knot)
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

-- We want to transform our list of instructions in a list of tail positions
-- And then count the number of unique values
solution :: Rope -> [Instruction] -> Int
solution r is =
    let rs       = execute [r] is
        (hs, ts) = unzip rs
    in length $ nub ts

-- Solution B --
-- Same as above, except that the rope is now a linked-list of knots 10 segments long

-- Our new ropes are made of Links, each either
--  A Knot, and End
-- or Knot, and more Rope
data Rope' = End Knot | Link Knot (Rope')
    deriving (Show)

-- Get the end of the rope!
tail' :: Rope' -> Rope'
tail' (End k)       = (End k)
tail' (Link _ next) = tail' next

-- Add a link to a list!
prepend :: Knot -> Rope' -> Rope'
prepend k (End k')       = Link k (End k')
prepend k (Link k' next) = Link k (Link k' next)

-- Get the knot from a piece of rope
knot (End k)    = k
knot (Link k _) = k

right' :: Rope' -> Rope'
right' (End (hx, hy)) = End (hx+1, hy)
right' (Link (hx, hy) t)
    | tx == hx   = Link (hx+1, hy) t
    | tx == hx+1 = Link (hx+1, hy) t
    | ty < hy    = Link (hx+1, hy) (upright' t)
    | ty > hy    = Link (hx+1, hy) (downright' t)
    | otherwise  = Link (hx+1, hy) (right' t)
    where (tx, ty) = knot t

-- ..H
-- .s.
-- ...
upright' :: Rope' -> Rope'
upright' (End (hx, hy)) = End (hx+1, hy+1)
upright' (Link (hx, hy) t)
    | tx > hx && ty < hy  = Link (hx+1, hy+1) (up' t)
    | tx < hx && ty > hy  = Link (hx+1, hy+1) (right' t)
    | tx < hx || ty < hy  = Link (hx+1, hy+1) (upright' t)
    | otherwise           = Link (hx+1, hy+1) t
    where (tx, ty) = knot t

-- ...
-- .s.
-- ..H
downright' :: Rope' -> Rope'
downright' (End (hx, hy)) = End (hx+1, hy-1)
downright' (Link (hx, hy) t)
    | tx > hx && ty > hy  = Link (hx+1, hy-1) (down' t)
    | tx < hx && ty < hy  = Link (hx+1, hy-1) (right' t)
    | tx < hx || ty > hy  = Link (hx+1, hy-1) (downright' t)
    | otherwise           = Link (hx+1, hy-1) t
    where (tx, ty) = knot t

left' :: Rope' -> Rope'
left' (End (hx, hy)) = End (hx-1, hy)
left' (Link (hx, hy) t)
    | tx == hx   = Link (hx-1, hy) t
    | tx == hx-1 = Link (hx-1, hy) t
    | ty < hy    = Link (hx-1, hy) (upleft' t)
    | ty > hy    = Link (hx-1, hy) (downleft' t)
    | otherwise  = Link (hx-1, hy) (left' t)
    where (tx, ty) = knot t

-- H..
-- .s.
-- ...
upleft' :: Rope' -> Rope'
upleft' (End (hx, hy)) = End (hx-1, hy+1)
upleft' (Link (hx, hy) t)
    | tx < hx && ty < hy  = Link (hx-1, hy+1) (up' t)
    | tx > hx && ty > hy  = Link (hx-1, hy+1) (left' t)
    | tx > hx || ty < hy  = Link (hx-1, hy+1) (upleft' t)
    | otherwise           = Link (hx-1, hy+1) t
    where (tx, ty) = knot t

-- ...
-- .s.
-- H..
downleft' :: Rope' -> Rope'
downleft' (End (hx, hy)) = End (hx-1, hy-1)
downleft' (Link (hx, hy) t)
    | tx < hx && ty > hy  = Link (hx-1, hy-1) (down' t)
    | tx > hx && ty < hy  = Link (hx-1, hy-1) (left' t)
    | tx > hx || ty > hy  = Link (hx-1, hy-1) (downleft' t)
    | otherwise           = Link (hx-1, hy-1) t
    where (tx, ty) = knot t

-- .H.
-- .s.
-- ...
up' :: Rope' -> Rope'
up' (End (hx, hy)) = End (hx, hy+1)
up' (Link (hx, hy) t)
    | ty == hy   = Link (hx, hy+1) t
    | ty == hy+1 = Link (hx, hy+1) t
    | tx < hx    = Link (hx, hy+1) (upright' t)
    | tx > hx    = Link (hx, hy+1) (upleft' t)
    | otherwise  = Link (hx, hy+1) (up' t)
    where (tx, ty) = knot t

-- ...
-- .s.
-- .H.
down' :: Rope' -> Rope'
down' (End (hx, hy)) = End (hx, hy-1)
down' (Link (hx, hy) t)
    | ty == hy   = Link (hx, hy-1) t
    | ty == hy-1 = Link (hx, hy-1) t
    | tx < hx    = Link (hx, hy-1) (downright' t)
    | tx > hx    = Link (hx, hy-1) (downleft' t)
    | otherwise  = Link (hx, hy-1) (down' t)
    where (tx, ty) = knot t

execute' :: [Rope'] -> [Instruction] -> [Rope']
execute' rs [] = rs
execute' (r:rs) (i:is) = case i of 
    ('L', 1) -> execute' ((left' r):r:rs) is
    ('L', n) -> execute' ((left' r):r:rs) (('L', n-1):is)
    ('R', 1) -> execute' ((right' r):r:rs) is
    ('R', n) -> execute' ((right' r):r:rs) (('R', n-1):is)
    ('U', 1) -> execute' ((up' r):r:rs) is
    ('U', n) -> execute' ((up' r):r:rs) (('U', n-1):is)
    ('D', 1) -> execute' ((down' r):r:rs) is
    ('D', n) -> execute' ((down' r):r:rs) (('D', n-1):is)
    (unk, n) -> execute' (r:rs) is

-- solution' :: Rope' -> [Instruction] -> Int
solution' r is =
    let rs = execute' [r] is
        ts = map (knot . tail') rs
    in length $ nub ts

-- Main --
parseLine :: String -> (Char, Int)
parseLine s =
    let h = head s
        t = read . last $ words s
    in (h, t)

-- (H, T) both at (0, 0)
startPos = ((0, 0), (0, 0))

-- Our old rope, but now as a linked list
toRope r [] = r
toRope r (_:xs) = toRope (prepend (0,0) r) xs
startPos' = toRope (End (0,0)) [0..8]

main = do
    input <- getContents
    print . (solution' startPos') $ map parseLine (lines input)
