import Data.Char

-- A tupple of Digits and Neighbours eg.
-- ("456", "..12.%...%..")
type GridNum = (String, String)

-- Given an x, y coordinate, find all neighbours
neighbours :: [String] -> Int -> Int -> [Char]
neighbours grid x y =
    let maxY = (length grid) - 1
        maxX = (length $ head grid) - 1
        rowAbove = if (y==0) then [] else grid !! (y-1)
        rowBelow = if (y>=maxY) then [] else grid !! (y+1)
        current = grid !! y
        leftRight = if (x==0) then [current !! (x+1)]
            else if (x>=maxX) then [current !! (x-1)]
            else [current !! (x-1), current !! (x+1)]
        above = if (x==0) then take 2 rowAbove else take 3 $ drop (x-1) rowAbove
        below = if (x==0) then take 2 rowBelow else take 3 $ drop (x-1) rowBelow
    in above ++ leftRight ++ below

-- Look for any non '.' or digit symbol in a list of neighbours
isValid :: String -> Bool
isValid [] = False
isValid (x:xs)
    | x=='.'    = isValid xs
    | isDigit x = isValid xs
    | otherwise = True

isValidGridNum :: GridNum -> Bool
isValidGridNum (_, ns) = isValid ns

validate :: [GridNum] -> [GridNum]
validate xs = filter isValidGridNum xs

-- I am quite sure that passing state around with this many arguments is bad practice
-- but here we are
parseRow :: [String] -> String -> Int -> Int -> [GridNum] -> [GridNum]
parseRow grid [] x y result = result
parseRow grid row@(d:ds) x y result@((n, ns):gs)
    | isDigit d = parseRow grid ds (x+1) y $ ((n ++ [d]), ns ++ (neighbours grid x y)):gs
    | otherwise = parseRow grid ds (x+1) y $ ("", ""):(n, ns):gs

parseGrid :: [String] -> [String] -> Int -> Int -> [GridNum] -> [GridNum]
parseGrid grid [] x y result          = result
parseGrid grid rows@(r:rs) x y result = parseGrid grid rs x (y+1) $ parseRow grid r x y result

parse :: [String] -> [GridNum]
parse grid = parseGrid grid grid 0 0 [("", "")]

solution_a :: String -> Int
solution_a input =
    let validated = validate . parse $ lines input
    in sum $ map (read . fst) validated

-- Main
main = do
    input <- getContents
    print $ solution_a input
