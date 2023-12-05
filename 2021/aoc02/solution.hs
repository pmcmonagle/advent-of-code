type Point = (Int, Int)
type Point' = (Int, Int, Int)
type Instruction = (String, Int)

translate :: [Instruction] -> Point -> Point
translate [] p = p
translate ((d, n):is) (x, y)
    | d == "up"   = translate is (x, y - n)
    | d == "down" = translate is (x, y + n)
    | otherwise   = translate is (x + n, y)

translate' :: [Instruction] -> Point' -> Point'
translate' [] p = p
translate' ((d, n):is) (x, y, a)
    | d == "up"   = translate' is (x, y, a - n)
    | d == "down" = translate' is (x, y, a + n)
    | otherwise   = translate' is (x + n, y + (a*n), a)

parse :: String -> [Instruction]
parse input = map parseLine $ lines input
    where parseLine l = let (h, t) = break (==' ') l in (h, read t)

solution_a :: String -> Int
solution_a input =
    let is = parse input
        (x, y) = translate is (0, 0)
    in x * y

solution_b :: String -> Int
solution_b input =
    let is = parse input
        (x, y, _) = translate' is (0, 0, 0)
    in x * y

main = do
    input <- getContents
    print $ solution_a input
    print $ solution_b input
