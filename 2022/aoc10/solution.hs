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

-- Main --
main = do
    input <- getContents
    print . solution $ map parse (lines input)
