-- Assume you have a boat with a button that makes it go
-- For ever millisecond you hold down the button, the boat travels 1 mm per ms faster
-- Give a list of race times and distances to win, find the number of
-- possible winning outcomes.
-- This can be mapped as a parabola:
--     y = -x^2 + Tx - D (where T is Time and D is Distance)
-- The number of winning outcomes are the number of positive y values under the parabola

-- There might be a mathier way to do this, but I think I can do it as a list comprehension
-- The maximum time given in my puzzle input is < 100, so it should be safe to take from 0..100
winningOutcomes :: Int -> Int -> [Int]
winningOutcomes t d = [ y | x <- [0..100], let y = -x^2 + (t*x) - d, y > 0]

-- Also the puzzle input is quite small, so I'm not going to bother parsing it
test  = [(7, 9), (15, 40), (30, 200)]
input = [(45, 295), (98, 1734), (83, 1278), (73, 1210)]

solution_a xs = product $ map solve xs
    where solve (t, d) = length $ winningOutcomes t d

-- Solution B is the same, but the input uses a single large set of numbers
test'  = (71530, 940200)
input' = (45988373, 295173412781210)

-- Solution B has a max time of... 45988373
winningOutcomes' :: Int -> Int -> [Int]
winningOutcomes' t d = [ y | x <- [0..45988373], let y = -x^2 + (t*x) - d, y > 0]

solution_b (t, d) = length $ winningOutcomes' t d

main = do
    print $ solution_a input
    print $ solution_b input'
