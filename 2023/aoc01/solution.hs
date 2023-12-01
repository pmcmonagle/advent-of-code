import Data.Char

-- Given a list of alphanumeric characters eg.
-- onextwo34five6abc
-- Convert written representations to digits; one -> 1, two -> 2 etc.
-- Grab the first and last digits, throwing out remaining letters
--   onextwo34five6abc -> 1x23456abc -> 1,6 -> 16
-- Do this for all the lines, and sum the result

-- GOTCHA! oneight should parse as 1 at the strart of a line, but 8 at the end

-- Stop once we find a digit, and start searching from the right
leftReplace :: String -> String
leftReplace ('1':xs) = '1' : (reverse . rightReplace $ reverse xs)
leftReplace ('2':xs) = '2' : (reverse . rightReplace $ reverse xs)
leftReplace ('3':xs) = '3' : (reverse . rightReplace $ reverse xs)
leftReplace ('4':xs) = '4' : (reverse . rightReplace $ reverse xs)
leftReplace ('5':xs) = '5' : (reverse . rightReplace $ reverse xs)
leftReplace ('6':xs) = '6' : (reverse . rightReplace $ reverse xs)
leftReplace ('7':xs) = '7' : (reverse . rightReplace $ reverse xs)
leftReplace ('8':xs) = '8' : (reverse . rightReplace $ reverse xs)
leftReplace ('9':xs) = '9' : (reverse . rightReplace $ reverse xs)
leftReplace ('o':'n':'e':xs) = '1' : (reverse . rightReplace $ reverse xs)
leftReplace ('t':'w':'o':xs) = '2' : (reverse . rightReplace $ reverse xs)
leftReplace ('t':'h':'r':'e':'e':xs) = '3' : (reverse . rightReplace $ reverse xs)
leftReplace ('f':'o':'u':'r':xs) = '4' : (reverse . rightReplace $ reverse xs)
leftReplace ('f':'i':'v':'e':xs) = '5' : (reverse . rightReplace $ reverse xs)
leftReplace ('s':'i':'x':xs) = '6' : (reverse . rightReplace $ reverse xs)
leftReplace ('s':'e':'v':'e':'n':xs) = '7' : (reverse . rightReplace $ reverse xs)
leftReplace ('e':'i':'g':'h':'t':xs) = '8' : (reverse . rightReplace $ reverse xs)
leftReplace ('n':'i':'n':'e':xs) = '9' : (reverse . rightReplace $ reverse xs)
leftReplace (x:xs) = x : leftReplace xs
leftReplace "" = ""

-- Stop once we find a digit, and return the result
rightReplace :: String -> String
rightReplace ('e':'n':'o':xs) = '1' : xs
rightReplace ('o':'w':'t':xs) = '2' : xs
rightReplace ('e':'e':'r':'h':'t':xs) = '3' : xs
rightReplace ('r':'u':'o':'f':xs) = '4' : xs
rightReplace ('e':'v':'i':'f':xs) = '5' : xs
rightReplace ('x':'i':'s':xs) = '6' : xs
rightReplace ('n':'e':'v':'e':'s':xs) = '7' : xs
rightReplace ('t':'h':'g':'i':'e':xs) = '8' : xs
rightReplace ('e':'n':'i':'n':xs) = '9' : xs
rightReplace (x:xs) = x : rightReplace xs
rightReplace "" = ""

-- This one fails due to oneight, eightwo, etc. parsing differently depending on whether or not
-- we've found the left-most digit
replaceStrings :: String -> String
replaceStrings ('o':'n':'e':xs) = '1' : replaceStrings xs
replaceStrings ('t':'w':'o':xs) = '2' : replaceStrings xs
replaceStrings ('t':'h':'r':'e':'e':xs) = '3' : replaceStrings xs
replaceStrings ('f':'o':'u':'r':xs) = '4' : replaceStrings xs
replaceStrings ('f':'i':'v':'e':xs) = '5' : replaceStrings xs
replaceStrings ('s':'i':'x':xs) = '6' : replaceStrings xs
replaceStrings ('s':'e':'v':'e':'n':xs) = '7' : replaceStrings xs
replaceStrings ('e':'i':'g':'h':'t':xs) = '8' : replaceStrings xs
replaceStrings ('n':'i':'n':'e':xs) = '9' : replaceStrings xs
replaceStrings (x:xs) = x : replaceStrings xs
replaceStrings "" = ""

firstLast :: [Int] -> Int
firstLast is = read $ (show $ head is) ++ (show $ last is)

getDigits :: [Char] -> [Char]
getDigits ds = filter isDigit $ leftReplace ds

parse :: String -> [[Int]]
parse cs = map (map digitToInt . getDigits) (lines cs)

-- Main
main = do
    input <- getContents
    print $ leftReplace "oneight12eightwo"
    print $ replaceStrings "oneight12eightwo"
    print $ (map firstLast) (parse input)
    print . sum $ (map firstLast) (parse input)
