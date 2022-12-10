-- Some data types to help us out
data Instruction = Noop | Add Int
    deriving (Show)

-- Tuple representing: (cycles, register)
type State = (Int, Int)

parse :: String -> Instruction
parse s
    | s == "noop" = Noop
    | otherwise   = Add (read . last $ words s)

execute :: [State] -> [Instruction] -> [State]
execute ss [] = ss
execute (s:ss) (i:is) = case i of
    Noop  -> execute ((c+1, r):s:ss) is
    Add x -> execute ((c+2, r+x):(c+1, r):s:ss) is
    where (c, r) = s

-- Solution A --
-- Given a list of instructions in the form of
-- noop | addx n
--
-- Execute these instructions such that
-- noop   -> wait 1 cycle
-- addx n -> wait 2 cycles & add n to register
--
-- Then, multiply each cycle by its register value to get a
-- "signal strength"
--
-- Determine the sum of the signal strength at the 20th cycle,
-- and every 40th cycle after that (so 20, 60, 100, 140, etc)

-- Start the program at cycle=1 so that we can get the value DURING the cycle
-- If we want the value after the cycle, we should start at cycle=0
startState = [(1, 1)]
testInstructions = [Noop, Add 3, Add (-5)]

-- We're looking for the sum of all signal strengths at c=20 and every 40th
-- cycle afterwards
solution :: [Instruction] -> Int
solution is = sum [c*r | (c,r) <- execute startState is, (c `rem` 40) - 20 == 0]

-- Solution B --
-- Apparently we are now drawing to the following grid where:
-- - each x position is drawn at x = (c-1) `rem` 40
-- - each y position is drawn at y = (c-1) `div` 40
-- Cycle   1 -> ######################################## <- Cycle  40
-- Cycle  41 -> ######################################## <- Cycle  80
-- Cycle  81 -> ######################################## <- Cycle 120
-- Cycle 121 -> ######################################## <- Cycle 160
-- Cycle 161 -> ######################################## <- Cycle 200
-- Cycle 201 -> ######################################## <- Cycle 240
--
-- Our register similarly represents a 3-pixel sprite where its x is the current
-- value of the register, and its y matches the cycle y
-- And it also covers (x-1, y) and (x+1, y) with its left and right respectively

-- Replace the character at (x, y) with c
replace :: [String] -> (Int, Int) -> Char -> [String]
replace crt (x, y) c =
    let (hy, ty) = splitAt y crt
        (hx, tx) = splitAt x $ head ty
        newRow   = hx ++ [c] ++ (tail tx)
    in hy ++ [newRow] ++ (tail ty)

-- Determine (cx, cy). If cx == r || r-1 || r+1 then draw #
--                     otherwise draw .
scan :: [String] -> State -> [String]
scan crt (c, r)
    | cx == r   = replace crt (cx, cy) '#'
    | cx == r-1 = replace crt (cx, cy) '#'
    | cx == r+1 = replace crt (cx, cy) '#'
    | otherwise = replace crt (cx, cy) '.'
    where (cx, cy) = ((c-1) `rem` 40, (c-1) `div` 40)

-- Draw each state in sequence until we get the final result
draw :: [String] -> [State] -> [String]
draw crt [] = crt
draw crt (s:ss) = draw (scan crt s) ss

startImage = [
    "........................................",
    "........................................",
    "........................................",
    "........................................",
    "........................................",
    "........................................"
    ]

-- Some quick formatting to make it look nice in the console
pretty [] = []
pretty (s:ss) = s ++ "\n" ++ (pretty ss)

solution' is = pretty . (draw startImage) $ execute startState is

-- Main --
main = do
    input <- getContents
    putStr . solution' $ map parse (lines input)
