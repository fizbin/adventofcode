-- stack --resolver nightly-2021-11-28 script --package mtl --package containers
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE LambdaCase #-}

import Data.List

--import Debug.Trace
import System.Environment

every :: Int -> [a] -> [a]
every _ [] = []
every n lst = head lst : every n (drop n lst)

foldX :: Int -> [(Int, Int)] -> [(Int, Int)]
foldX val dots =
  nub $
  [(x, y) | (x, y) <- dots, x < val] ++
  [(2 * val - x, y) | (x, y) <- dots, x > val]

foldY :: Int -> [(Int, Int)] -> [(Int, Int)]
foldY val dots =
  nub $
  [(x, y) | (x, y) <- dots, y < val] ++
  [(x, 2 * val - y) | (x, y) <- dots, y > val]

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc13.in"
          else head args
  (datas, foldstrs) <- break (== "") . lines <$> readFile filename
  let points =
        map (\s -> read ("(" ++ s ++ ")")) (words $ intercalate "\n" datas)
  let folds = every 3 $ drop 2 $ words $ intercalate "\n" foldstrs
  let dofold =
        \case
          ('x':'=':s) -> foldX (read s)
          ('y':'=':s) -> foldY (read s)
          f -> error $ "Bad fold: " ++ f
  let fold1 = dofold (head folds) points
  print $ length fold1
  let finalpoints = foldl' (flip dofold) points folds
  let mX = maximum $ map fst finalpoints
  let mY = maximum $ map snd finalpoints
  mapM_
    putStrLn
    ([ (\x ->
          if (x, y) `elem` finalpoints
            then '#'
            else '.') <$>
     [0 .. mX]
     | y <- [0 .. mY]
     ])
