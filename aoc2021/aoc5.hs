{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import Data.List (sort)
import System.Environment

every :: Int -> [a] -> [a]
every _ [] = []
every n lst = head lst : every n (drop n lst)

spotsfor :: (Enum a, Ord a, Num a) => ((a, a), (a, a)) -> [(a, a)]
spotsfor ((x1, y1), (x2, y2))
  | x1 == x2 =
    let [a, b] = sort [y1, y2]
     in [(x1, g) | g <- [a .. b]]
spotsfor ((x1, y1), (x2, y2))
  | y1 == y2 =
    let [a, b] = sort [x1, x2]
     in [(g, y1) | g <- [a .. b]]
spotsfor ((x1, y1), (x2, y2)) =
  let dir1 = if x1 < x2 then 1 else -1
      dir2 = if y1 < y2 then 1 else -1
      distlim = abs (x1-x2)
  in [(x1 + dist*dir1, y1 + dist*dir2) | dist <- [0..distlim]]

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc5.in" else head args
  strs <- words <$> readFile filename
  let startstrs = every 3 strs
  let endstrs = every 3 (drop 2 strs)
  let starts = [(x, y) | s <- startstrs, ([x, y], "") <- reads ("[" ++ s ++ "]")]
  let ends = [(x, y) | s <- endstrs, ([x, y], "") <- reads ("[" ++ s ++ "]")]
  let mylines = zip starts ends :: [((Int, Int), (Int, Int))]
  let hlines = [l | l <- mylines, (\((a,_),(c,_)) -> a==c) l]
  let vlines = [l | l <- mylines, (\((_,b),(_,d)) -> b==d) l]
  let m = M.fromListWith (+) $ map (,1::Int) $ concatMap spotsfor (hlines ++ vlines)
  print $ length [x | (x, y) <- M.toList m, y > 1]
  let m2 = M.fromListWith (+) $ map (,1::Int) $ concatMap spotsfor mylines
  print $ length [x | (x, y) <- M.toList m2, y > 1]
