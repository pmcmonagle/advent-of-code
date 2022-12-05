-- Just easier to define the starting stacks in code
-- than to parse them out of the file
testStacks = [
        "ZN",
        "MCD",
        "P"
    ]

inputStacks = [
        "BQC",
        "RQWZ",
        "BMRLV",
        "CZHVTW",
        "DZHBNVG",
        "HNPCJFVQ",
        "DGTRWZS",
        "CGMNBWZP",
        "NJBMWQFP"
    ]

-- Solution A --
-- Given a set of stacks eg.
-- 1 : Z, N
-- 2 : M, C, D
-- 3 : P
-- 
-- Parse sets of instructions eg.
-- move 1 from 2 to 1
-- Such that the stacks are transformed to fit
-- the result
-- 1 : Z, N, D
-- 2 : M, C
-- 3 : P
--
-- Print the top character in each stack after all transformations

-- Given a list strings, replace the value at index x with v
replace :: [String] -> Int -> String -> [String]
replace ss x v = hs ++ [v] ++ (tail ts)
    where (hs, ts) = splitAt x ss

-- Move an element in ss from a to b
move :: [String] -> Int -> Int -> [String]
move ss a b =
    let (_, as) = splitAt a ss
        (_, bs) = splitAt b ss
        newA    = init $ head as
        newB    = (head bs) ++ [last $ head as]
    in replace (replace ss a newA) b newB

-- Execute a single instruction, the instruction
-- must be split into words first, eg. ["move", "1", "from", "2", "to", "1"]
execute :: [String] -> [String] -> [String]
execute ss (_:"0":_) = ss -- identity
execute ss (_:n:_:a:_:b:_) = execute (move ss (read a - 1) (read b - 1)) ["move", show ((read n) - 1), "from", a, "to", b]

solution :: [String] -> [String] -> [String]
solution ss [] = ss -- identity
solution ss (i:is) = solution (execute ss $ words i) is


-- Solution B --


-- Main --
main = do
    input <- getContents
    print . map last $ solution inputStacks (lines input)
