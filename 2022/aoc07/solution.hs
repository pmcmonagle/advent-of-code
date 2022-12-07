-- Give a list of instructions which can take the form of
-- $ cd /  -- move to root directory
-- $ cd x  -- move one directory down the tree, to x
-- $ cd .. -- move one directory up the tree
-- $ ls    -- list all contents of a directory
-- 
-- and outputs which can take the form of
-- dir x   -- a directory named x
-- 123 y   -- a file, y, with filesize 123
--
-- Build a Tree to describe the directory structure
-- such that you will be able to calculate the total filesize
-- of any directory (including all subdirectories)

-- Start with a data type defining directories that can
-- hold a list of files or subdirectories
data Directory = Root [Int] [Directory]
    | Dir String [Int] [Directory]

instance Show Directory where
    show (Root fs ds) = "Root files:" ++ show fs ++ " \n\tdirs:" ++ show ds
    show (Dir n fs ds) = n ++ " files:" ++ show fs ++ " \n\tdirs:" ++ show ds

name :: Directory -> String
name (Root _ _) = "root"
name (Dir n _ _) = n

-- Add a file to a directory
addFile :: Directory -> Int -> Directory
addFile (Root fs ds) f = Root (f:fs) ds
addFile (Dir name fs ds) f = Dir name (f:fs) ds

-- Add a subdirectory to a directory
addDir :: Directory -> String -> Directory
addDir (Root fs ds) n = Root fs ((Dir n [] []):ds)
addDir (Dir name fs ds) n = Dir name fs ((Dir n [] []):ds)

-- Get a child directory of a given name
getChild :: Directory -> String -> Directory
getChild (Root _ ds) x = head $ dropWhile (\d -> name d /= x) ds
getChild (Dir _ _ ds) x = head $ dropWhile (\d -> name d /= x) ds

-- Replace a child directory by name with a new version
replaceChild :: Directory -> String -> Directory -> Directory
replaceChild (Root fs ds) n' d' = Root fs (hs ++ [d'] ++ (tail ts))
    where (hs, ts) = break (\d -> name d == n') ds
replaceChild (Dir n fs ds) n' d' = Dir n fs (hs ++ [d'] ++ (tail ts))
    where (hs, ts) = break (\d -> name d == n') ds

-- Given a list of instructions as described above, 
-- build out our directory tree
parse :: [String] -> Directory -> [Directory] -> Directory
parse [] cd [] = cd
parse [] cd path = parse ["$ cd .."] cd path
parse (x:xs) cd path
    | x == "$ cd /"  = parse xs cd []
    | x == "$ ls"    = parse xs cd path
    | x == "$ cd .." = parse xs (replaceChild (head path) (name cd) cd) (tail path)
    -- $ cd x
    | head x == '$'  = let name = last (words x) in parse xs (getChild cd name) (cd:path)
    -- dir x
    | head x == 'd'  = let name = last (words x) in parse xs (addDir cd name) path
    -- 123 y
    | otherwise      = let size = read . head $ words x in parse xs (addFile cd size) path

-- Solution A --
-- For the first solution, find all directories with a size < 100,000
-- and calculate the sums of their sizes

-- Get cumulative file size for a directory and all subdirectories
getFileSize :: Directory -> Int
getFileSize (Root [] []) = 0
getFileSize (Root fs ds) = sum fs + sum (map getFileSize ds)
getFileSize (Dir _ [] []) = 0
getFileSize (Dir _ fs ds) = sum fs + sum (map getFileSize ds)

getDirSizes :: Directory -> [Int]
getDirSizes r@(Root _ ds) = (getFileSize r):(concat $ map getDirSizes ds)
getDirSizes d@(Dir _ _ ds) = (getFileSize d):(concat $ map getDirSizes ds)

maxFileSize = 100000

solution :: Directory -> Int
solution d = sum [s | s <- getDirSizes d, s < maxFileSize]

-- Solution B --

-- Main --
main = do
    input <- getContents
    print . solution $ parse (lines input) (Root [] []) []
