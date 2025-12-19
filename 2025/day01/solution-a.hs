-- Given a list of commands, eg. L12, R31, L14
-- Map each command to an addition or subtraction on
-- a "wheel" numbered 0-99.
-- eg. if starting at 50, L10 = 40 and R10 = 60
-- They should be executed in sequence, eg. L10,R10,L20 = 40, 50, 30
-- Then, count the number of times we hit 0

data Direction = L | R
    deriving (Show)

toDirection :: Char -> Direction
toDirection 'L' = L
toDirection 'R' = R
toDirection any = R

type Instruction = (Direction, Int)

-- Given a starting position, execute a single instruction
transform :: Int -> Instruction -> Int
transform x (L, n) = (x - n) `mod` 100
transform x (R, n) = (x + n) `mod` 100

-- Given a starting position and a set of instructions, execute each
execute :: Int -> [Instruction] -> [Int]
execute n [] = []
execute n (x:xs) = execute result xs ++ [result]
    where result = transform n x

parse :: String -> [Instruction]
parse input = map parseInstruct $ lines input
    where parseInstruct (x:xs) = (toDirection x, read xs)

solve :: [Instruction] -> Int
solve xs =
    let results = execute 50 xs
        filtered = filter (== 0) results
    in length filtered

-- Get the problem input, parse it, and solve the problem
main = do
    input <- getContents
    print $ solve $ parse input
