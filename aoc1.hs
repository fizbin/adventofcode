import System.Environment

p1 :: [Int] -> Int
p1 ilist = length [x | (x, y) <- zip ilist (tail ilist), x < y]

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc1.in" else head args
  s <- readFile filename
  let ilist = map read (words s) :: [Int]
  print $ p1 ilist
  print $ p1 [x + y + z | (x,y,z) <- zip3 (tail ilist) (tail $ tail ilist) ilist]