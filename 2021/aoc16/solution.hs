import Data.Char

-- Type Definitions, for clarity
type Bounds = ((Integer, Integer), (Integer, Integer))
type Ticket = [Integer]

-- Used to split a string with a delimiter 
-- eg. split ',' "a,b,c" -> ["a","b","c"]
split :: Char -> String -> [String]
split c s = case dropWhile (==c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (==c) s'

-- For Parsing Tickets
values :: String -> Ticket
values s = map read xs
  where xs = split ',' s

tickets :: String -> [Ticket]
tickets s = map values xs
  where xs = drop 1 $ dropWhile (/= "nearby tickets:") $ lines s

getMyTicket :: String -> Ticket
getMyTicket s = values t
  where t = head $ drop 1 $ dropWhile (/= "your ticket:") $ lines s

-- For Parsing Rules
bounds :: String -> Bounds
bounds s = (toTuple $ map read first, toTuple $ map read second)
  where vals = drop 2 $ dropWhile (/= ':') s
        first = split '-' $ takeWhile (/= ' ') vals
        second = split '-' $ drop 2 $ dropWhile (/= 'r') vals
        toTuple [a,b] = (a,b)

rules :: String -> [Bounds]
rules s = map bounds xs
  where xs = takeWhile (/= "") $ lines s

-- Return True if a value is within the provided bounds
isInBounds :: Integer -> Bounds -> Bool
isInBounds n ((a,b), (c,d)) = (n >= a && n <= b) || (n >= c && n <= d)

-- Return True if a value is within at least one of the provided bounds
isInAnyBounds :: [Bounds] -> Integer -> Bool
isInAnyBounds bs n = foldl (||) False $ map (isInBounds n) bs

-- Return True if every ticket value is in any bounds
ticketIsValid :: [Bounds] -> Ticket -> Bool
ticketIsValid bs ts = foldl (&&) True $ map (isInAnyBounds bs) ts

-- Solution 1, Get a list of all values that are not in any bounds
allOutOfBounds :: [Ticket] -> [Bounds] -> [Integer]
allOutOfBounds ts bs = filter (not . (isInAnyBounds bs)) $ concat ts

main = do
  input <- getContents
  let myTicket = getMyTicket input
      validTickets = filter (ticketIsValid $ rules input) (tickets input)
  print validTickets
