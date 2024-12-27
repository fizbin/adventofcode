{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Data.Bits ((.^.))
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

advance :: Int -> Int
advance =
  let mixprune op n = (op n .^. n) `mod` 16777216
   in mixprune (* 2048) . mixprune (`div` 32) . mixprune (* 64)

last4 :: [a] -> [Maybe (a, a, a, a)]
last4 = go0
  where
    go0 [] = []
    go0 (a : as) = Nothing : go1 a as
    go1 _ [] = []
    go1 a (b : bs) = Nothing : go2 a b bs
    go2 _ _ [] = []
    go2 a b (c : cs) = Nothing : go3 a b c cs
    go3 _ _ _ [] = []
    go3 a b c (d : ds) = Just (a, b, c, d) : go3 b c d ds

part2 :: [Int] -> Int
part2 snums =
  let valrow s = take 2001 $ map (`mod` 10) $ iterate advance s
      diffrow s = zipWith (-) (tail $ valrow s) (valrow s)
      mapVals s = mapMaybe (\(x, y) -> (,y) <$> x) $ zip (last4 (diffrow s)) (tail $ valrow s)
      rowMap s = let q = M.fromListWith (const id) $ mapVals s in q
      fullmap = M.unionsWith (+) $ rowMap <$> snums
   in M.foldl' max 0 fullmap

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc22.in" else head args
  mydata <- map read . lines <$> readFile filename :: IO [Int]
  putStr "Part 1: "
  print $ sum $ [iterate advance v !! 2000 | v <- mydata]
  putStr "Part 2: "
  print $ part2 mydata
