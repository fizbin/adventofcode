import Control.Monad (when)
import Data.Array ((!))
import Data.Array qualified as A
import Data.List (foldl', nub)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import System.Environment (getArgs)

elfDistanceMap :: [[Char]] -> (Int, Int) -> [[Maybe Int]]
elfDistanceMap grid startpoint =
  mkGrid $ go (M.fromList [(startpoint, 0)]) [startpoint]
 where
  gridM = M.fromList [((i, j), ch) | (i, row) <- zip [0 ..] grid, (j, ch) <- zip [0 ..] row]
  go :: Map (Int, Int) Int -> [(Int, Int)] -> Map (Int, Int) Int
  go from boundary =
    let bd2 =
          [ ((iN, jN), n + 1)
          | (i, j) <- boundary
          , (iN, jN) <- [(i + 1, j), (i, j + 1), (i - 1, j), (i, j - 1)]
          , Just '.' == M.lookup (iN, jN) gridM || Just 'S' == M.lookup (iN, jN) gridM
          , Just n <- [M.lookup (i, j) from]
          ]
        from2 = foldl' (\mp (s, n) -> M.insertWith min s n mp) from bd2
        bd3 = [s | (s, n) <- bd2, Just n == M.lookup s from2]
     in if null bd3 then from2 else go from2 (nub bd3)
  mkGrid :: Map (Int, Int) Int -> [[Maybe Int]]
  mkGrid mp = [[M.lookup (i, j) mp | (j, _) <- zip [0 ..] row] | (i, row) <- zip [0 ..] grid]

elfdistgrid :: [[Char]] -> A.Array (Ordering, Ordering) [[Maybe Int]]
elfdistgrid grid = A.listArray bounds [elfDistanceMap grid (corner spot) | spot <- A.range bounds]
 where
  bounds = ((LT, LT), (GT, GT))
  height = length grid
  width = length $ head grid
  corner (i, j) =
    let x = case i of
          LT -> 0
          EQ -> height `div` 2
          GT -> height - 1
        y = case j of
          LT -> 0
          EQ -> width `div` 2
          GT -> width - 1
     in (x, y)

doBlock :: Int -> Int -> A.Array (Ordering, Ordering) [[Maybe Int]] -> Int -> (Int, Int) -> Int
doBlock height width edg bigSteps =
  \(bigUD, bigLR) ->
    let effRem = eRemain (bigUD, bigLR)
     in if effRem < 0
          then 0
          else doCalc' effRem (bigUD `compare` 0, bigLR `compare` 0)
 where
  maxremain' = fromJust $ maximum [val | grid <- A.elems edg, row <- grid, val <- row]
  maxremain = maxremain' + 2 + (maxremain' `mod` 2)
  eRemain (bigUD, bigLR) =
    let
      vertsteps = case abs bigUD of
        0 -> 0
        x -> height * (x - 1) + 1 + (height `div` 2)
      horizsteps = case abs bigLR of
        0 -> 0
        y -> width * (y - 1) + 1 + (width `div` 2)
      remaining = bigSteps - vertsteps - horizsteps
     in
      if remaining > maxremain then maxremain - (remaining `mod` 2) else remaining
  doCalc remaining (ordUD, ordLR) =
    let relgrid = edg ! (ordUD, ordLR)
     in length
          [ () | row <- relgrid, val <- row, Just True == ((\x -> even (x + remaining) && (x <= remaining)) <$> val)
          ]
  memoBounds = ((0, LT, LT), (maxremain, GT, GT))
  memoArray = A.listArray memoBounds [doCalc remaining (a, b) | (remaining, a, b) <- A.range memoBounds]
  doCalc' remaining (ordUD, ordLR) = memoArray ! (remaining, ordUD, ordLR)

doRow :: Int -> Int -> Int -> Int -> ((Int, Int) -> Int) -> Int
doRow height width bigUD bigSteps subjob =
  let vertsteps = case abs bigUD of
        0 -> 0
        x -> height * (x - 1) + 1 + (height `div` 2)
      reachLR = ((bigSteps - vertsteps) + (width `div` 2)) `div` width
      (breakL, breakR) = if reachLR <= 4 then (0, 1) else (-reachLR + 2, reachLR - 3)
      total =
        sum $
          [subjob (bigUD, lr) | lr <- [(negate reachLR) .. breakL] ++ [breakR .. reachLR]]
            ++ [((breakR - breakL) `div` 2) * subjob (bigUD, lr) | lr <- [breakL + 1, breakL + 2]]
   in if bigSteps < vertsteps then 0 else total

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc21.in" else head args
  s <- lines <$> readFile filename
  let sSpot = head $ [(i, j) | (i, row) <- zip [0 ..] s, (j, ch) <- zip [0 ..] row, ch == 'S']
  let eDist5 = elfDistanceMap s sSpot
  print $ length [() | row <- eDist5, val <- row, Just True == ((\x -> even x && (x <= 64)) <$> val)]
  let height = length s
      width = length $ head s
  when (fst sSpot /= (height `div` 2)) $ ioError (userError "assertion failed")
  when (snd sSpot /= (width `div` 2)) $ ioError (userError "assertion failed")
  let bigSteps = 26501365 :: Int
  let edg = elfdistgrid s
      subjob = doBlock height width edg bigSteps
  print $
    sum
      [ doRow height width ud bigSteps subjob
      | ud <- [-2 - (bigSteps `div` length s) .. 2 + (bigSteps `div` length s)]
      ]