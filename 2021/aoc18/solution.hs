{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import qualified Data.Text as Text
import Language.Haskell.Interpreter

-- Used to split a string with a delimiter
-- eg. split ',' "a,b,c" -> ["a","b","c"]
split :: Char -> String -> [String]
split c s = case dropWhile (==c) s of
  "" -> []
  s' -> w : split c s''
    where (w, s'') = break (==c) s'

-- Parse Homework
-- NEW PLAN JUST REDEFINE * AND + IT WORKS
-- data Homework a = String a | List [Homework a]
-- conc :: [String] -> Homework a -> Homework a
-- conc xs hw = hw
-- parseHomework :: String -> Homework a
-- parseHomework [] = List []
-- parseHomework s = conc (words x) (List [parseHomework xs'])
--   where (x,xs) = break (=='(') s
--         xs' = takeWhile (/= ')') $ tail' xs
--         tail' [] = []
--         tail' xs = tail xs

convert :: String -> String
convert s = replaceAdd $ replaceMult s
  where replaceAdd = Text.unpack . Text.replace "+" "`add`" . Text.pack
        replaceMult = Text.unpack . Text.replace "*" "`mult`" . Text.pack

-- These are in arith.hs
add a b = a + b
mult a b = a * b

ex :: Interpreter ()
ex = do
  setImports ["Prelude"]
  loadModules ["arith.hs"]
  setTopLevelModules ["Arithmetic"]

evaluate s = do
  r <- runInterpreter $ ex >> eval (convert s)
  case r of
    Left err -> return "error"
    Right a -> return a

main = do
  input <- getContents
  let test1 = "2 * 3 + (4 * 5)"
      test2 = "5 + (8 * 3 + 9 + 3 * 4 * 3)"
      test3 = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
      test4 = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
      allHomework = lines input
  --output <- evaluate test2
  --print output
  output <- mapM evaluate allHomework
  print $ sum $ map read output

