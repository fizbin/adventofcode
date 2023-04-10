{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import System.Environment

histogram :: (Ord a, Num b) => [a] -> M.Map a b
histogram = M.fromListWith (+) . map (,1)

doOneStep :: (Ord a, Num a, Num b) => M.Map a b -> M.Map a b
doOneStep = M.fromListWith (+) . concatMap go . M.toList
  where
    go (0, x) = [(6, x), (8, x)]
    go (n, x) = [(n-1, x)]

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc6.in" else head args
  strs <- words <$> readFile filename
  let states = read ("[" ++ head strs ++ "]") :: [Int]
  let dayCounts = map (sum . map snd . M.toList) $ iterate doOneStep (histogram states) :: [Integer]
  print $ dayCounts !! 80
  print $ dayCounts !! 256