#!/usr/bin/env stack
-- stack --resolver lts-18.18 script --package multiset --package containers
{-# LANGUAGE Haskell2010 #-}

import Data.List
import Data.Maybe
import Debug.Trace
import System.Environment
import qualified Data.Map as M
import qualified Data.MultiSet as MS

p1 :: [((Int, Int), [(Int, Int)])] -> [((Int, Int), Char)] -> Int
p1 nbmap dat = sum doit
  where
    idat = M.fromList $ map (\(a, b) -> (a, read [b])) dat
    datfor c = fromJust $ M.lookup c idat
    doit = do
      (coord, nbs) <- nbmap
      let mydat = datfor coord
      pure $
        if all (\c -> mydat < datfor c) nbs
          then mydat + 1
          else 0

p2 :: [((Int, Int), [(Int, Int)])] -> [((Int, Int), Char)] -> Int
p2 nbmap dat =
  let finalMap = go initialBasins
      basincount =
        map snd $
        MS.toOccurList $ MS.fromList $ filter (/= 0) $ map snd finalMap
   in product . take 3 . reverse . sort $ basincount
  where
    initialBasins =
      zipWith
        (\(a, b) i ->
           if b == '9'
             then (a, 0)
             else (a, i))
        dat
        [1 ..]
    go :: [((Int, Int), Int)] -> [((Int, Int), Int)]
    go basins = do
      let basinMap = M.fromList basins
          consider nbtuple basintuple =
            let nbvals = map (fromJust . flip M.lookup basinMap) (snd nbtuple)
             in if snd basintuple == 0
                  then basintuple
                  else (fst basintuple, maximum $ snd basintuple : nbvals)
      let nbasins = zipWith consider nbmap basins
      if basins == nbasins
        then basins
        else go nbasins

mkCoordsWithNeighbors rows cols =
  [ ((i, j), nbs)
  | i <- [0 .. rows - 1]
  , j <- [0 .. cols - 1]
  , nbs <-
      [ [ (p, q)
        | (p, q) <- [(i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)]
        , 0 <= p
        , 0 <= q
        , p < rows
        , q < cols
        ]
      ]
  ]

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc9.in"
          else head args
  datas <- words <$> readFile filename
  let coordsWithNeighbors =
        mkCoordsWithNeighbors (length datas) (length $ head datas)
  let allspots = concat datas
  let datamap = zip (map fst coordsWithNeighbors) allspots
  print $ p1 coordsWithNeighbors datamap
  print $ p2 coordsWithNeighbors datamap
