{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Environment (getArgs)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
-- import Debug.Trace

type Coords3 = (Int, Int, Int)
type Coords4 = (Int, Int, Int, Int)

class Ord s => CoordType s where
  globalNeighborhood :: Set s -> [s]
  neighborCount :: s -> Set s -> Int
  makeFromGrid :: Int -> Int -> s

instance CoordType Coords3 where
  globalNeighborhood coordSet =
    S.toList $ S.fromList $
    [(x + xd, y + yd, z + zd) |
      (x, y, z) <- S.toList coordSet,
      xd <- [-1, 0, 1],
      yd <- [-1, 0, 1],
      zd <- [-1, 0, 1]]
  neighborCount spot coordSet =
    let (x, y, z) = spot
        in sum [if (x + xd, y + yd, z + zd) `S.member` coordSet then 1 else 0
               | xd <- [-1, 0, 1],
                 yd <- [-1, 0, 1],
                 zd <- [-1, 0, 1]]
    - (if spot `S.member` coordSet then 1 else 0)
  makeFromGrid x y = (x, y, 0)


instance CoordType Coords4 where
  globalNeighborhood coordSet =
    S.toList $ S.fromList $
    [(x + xd, y + yd, z + zd, w + wd) |
      (x, y, z, w) <- S.toList coordSet,
      xd <- [-1, 0, 1],
      yd <- [-1, 0, 1],
      zd <- [-1, 0, 1],
      wd <- [-1, 0, 1]]
  neighborCount spot coordSet =
    let (x, y, z, w) = spot
        in sum [if (x + xd, y + yd, z + zd, w + wd) `S.member` coordSet
                then 1 else 0
               | xd <- [-1, 0, 1],
                 yd <- [-1, 0, 1],
                 zd <- [-1, 0, 1],
                 wd <- [-1, 0, 1]]
    - (if spot `S.member` coordSet then 1 else 0)
  makeFromGrid x y = (x, y, 0, 0)

doProblem :: forall ct. CoordType ct => String -> Proxy ct -> IO ()
doProblem src _ = do
  let runGeneration coordSet =
        let nbhd = globalNeighborhood coordSet
            xform spot =
              let neighbors = neighborCount spot coordSet
              in if S.member spot coordSet
                 then [spot | neighbors == 2 || neighbors == 3]
                 else [spot | neighbors == 3]
        in
          S.fromList $ concatMap xform nbhd
      srcLines = lines src
      initial = concatMap (
        \(idx1, line) -> concatMap (
          \(idx2, char) -> [makeFromGrid idx1 idx2 | char == '#'])
                         (zip [0..] line)) (zip [0..] srcLines)
      initialSet :: Set ct
      initialSet = S.fromList initial
      
  print (S.size $ iterate runGeneration initialSet !! 6)

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc17.in" else head args
  s <- readFile filename
  doProblem s (Proxy @Coords3)
  doProblem s (Proxy @Coords4)
