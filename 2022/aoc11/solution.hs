import Data.List
import Data.Int

-- From Data.List.Split
-- Split a list on some predicate
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)

-- Let's parse out a useable data structure first
-- We need to construct a list of "Monkeys".
-- Each Monkey has:
--  - A list of items [Integer]
--  - An operation to perform ((Integer -> Integer -> Integer), Integer | Old)
--  - A test that follows the operation (Integer, Integer, Integer)
--  - A running tally of inspections performed, Integer
data Operation = Op Char Integer | Self Char
    deriving (Show)
data Monkey = Mon [Integer] Operation (Integer, Int, Int) Integer
    deriving (Show)

perf :: Operation -> Integer -> Integer
perf (Op '*' n) m = n * m
perf (Op c n) m = n + m
perf (Self '*') m = m * m
perf (Self c) m = m + m

parseOp :: String -> Operation
parseOp s =
    let (_, ts) = splitAt 23 s
        op      = head ts
        val     = last $ words ts
    in case val of
        "old" -> Self op
        _     -> Op op $ read val

parse :: [String] -> [Monkey]
parse [] = []
parse ls =
    let (hs, ts) = splitAt 7 ls
        items = map read $ splitOn (==',') (drop 18 $ hs !! 1)
        op = parseOp (hs !! 2)
        test = (read . last $ words (hs !! 3), read . last $ words (hs !! 4), read . last $ words (hs !! 5))
        m = Mon items op test 0
    in m:(parse ts)

-- Lets define some functions to transform our list of monkeys
throw :: [Monkey] -> Int -> [Monkey]
throw ms n =
    let (hs, ts) = splitAt n ms
        Mon (i:is) o t c = head ts
    in hs ++ [Mon is o t (c+1)] ++ (tail ts)

give :: [Monkey] -> Int -> Integer -> [Monkey]
give ms n val =
    let (hs, ts) = splitAt n ms
        Mon is o t c = head ts
    in hs ++ [Mon (is ++ [val]) o t c] ++ (tail ts)

inspect :: [Monkey] -> Int -> [Monkey]
inspect ms n
    | is == []         = ms
    | val `rem` d == 0 = inspect (throw (give ms t val) n) n
    | otherwise        = inspect (throw (give ms f val) n) n
    where
        (hs, ts) = splitAt n ms
        Mon is o (d, t, f) c = head ts
        val = (perf o $ head is)-- `div` 3

-- This is dirty and I bet it wouldn't be if I understood monads
doRound ms n
    | n >= (length ms) = ms
    | otherwise        = doRound (inspect ms n) (n+1)

-- Same as above, but counting down from n
doRounds ms n
    | n == 0    = ms
    | otherwise = doRounds (doRound ms 0) (n-1)

-- Get the inspections value from a monkey
getInspections (Mon is o t c) = c

-- Solution A --
-- Run 20 rounds of monkey business
-- Then find the inspection counts and multiply the two highest together

solution ms =
    let result = doRounds ms 20
        is = map getInspections result
    in product $ take 2 (reverse $ sort is)

-- Solution B --
-- Comment out the `div` 3 in inspect, and then 
-- run it 10000 times
-- Using Int doesn't have enough precision, but Integer is SO slow ;_;

solution' ms =
    let result = doRounds ms 10000
        is = map getInspections result
    in product $ take 2 (reverse $ sort is)


-- Main --
main = do
    input <- getContents
    print . solution' . parse $ lines input
