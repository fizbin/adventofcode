{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import Data.List (sort)
import System.Environment

every _ [] = []
every n lst = head lst : every n (drop n lst)

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
  let lines = zip starts ends :: [((Int, Int), (Int, Int))]
  let hlines = [l | l <- lines, (\((a,b),(c,d)) -> a==c) l]
  let vlines = [l | l <- lines, (\((a,b),(c,d)) -> b==d) l]
  let m = M.fromListWith (+) $ map (,1) $ concatMap spotsfor (hlines ++ vlines)
  print $ length [x | (x, y) <- M.toList m, y > 1]
  let m2 = M.fromListWith (+) $ map (,1) $ concatMap spotsfor lines
  print $ length [x | (x, y) <- M.toList m2, y > 1]
