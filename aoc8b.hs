{-# LANGUAGE Haskell2010 #-}

import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Environment

isSub [] _ = True
isSub _ [] = False
isSub (a:as) (b:bs)
  | a < b = False
  | a == b = isSub as bs
  | a > b = isSub (a : as) bs
  | otherwise = error "Trichotomy!"

p2 :: ([String], [String]) -> Int
p2 (ins, outs) =
  let [poss] = go
      mapish = zip poss [0 ..]
      intstr = concatMap (show . fromJust . flip lookup mapish) outs
   in read intstr
  where
    go :: [[String]]
    go = do
      let inlen x = filter ((== x) . length) ins
      [val0, val6, val9] <- permutations $ inlen 6
      [val2, val3, val5] <- permutations $ inlen 5
      let assign = [val0] ++ inlen 2 ++ [val2, val3] ++ inlen 4 ++
                   [val5, val6] ++ inlen 3 ++ inlen 7 ++ [val9]
      guard $ (assign !! 3) `isSub` (assign !! 9)
      guard $ (assign !! 5) `isSub` (assign !! 9)
      guard $ (assign !! 5) `isSub` (assign !! 6)
      pure assign

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc8.in"
          else head args
  mylines <- lines <$> readFile filename
  let parsem puzzle =
        let ws = words puzzle
            ins = map sort $ take 10 ws
            outs = map sort $ drop 11 ws
         in if null ws
              then Nothing
              else Just (ins, outs)
  let puzzles = mapMaybe parsem mylines
  let p1 =
        length $
        filter (\a -> length a `elem` [2, 4, 3, 7]) $ concatMap snd puzzles
  print p1
  let vals = map p2 puzzles
  print $ sum vals
