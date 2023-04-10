import System.Environment
import Data.List

lists :: Int -> [a] -> [[a]]
lists n src = go (take n src) (drop n src)
  where
    go val [] = [val]
    go val (s:ss) = val : go (tail val ++ [s]) ss

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc6.in" else head args
    s <- readFile filename
    print $ 4 + length (takeWhile ((4 /=) . length . nub) (lists 4 s))
    print $ 14 + length (takeWhile ((14 /=) . length . nub) (lists 14 s))
