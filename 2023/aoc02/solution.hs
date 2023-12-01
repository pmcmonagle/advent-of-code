import Data.Char

-- Problem

parse :: String -> [String]
parse xs = lines xs

-- Main
main = do
    input <- getContents
    print $ parse input
