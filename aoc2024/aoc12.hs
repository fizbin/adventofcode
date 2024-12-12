{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Arrow (Arrow (second))
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Bits (Bits ((.&.)))
import Data.Map qualified as M
import System.Environment (getArgs)

neighbors :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [(Int, Int)]
neighbors bds spot =
  filter
    (inRange bds)
    [ (fst spot - 1, snd spot),
      (fst spot + 1, snd spot),
      (fst spot, snd spot - 1),
      (fst spot, snd spot + 1)
    ]

getRegions :: (Eq a) => [[a]] -> [[Int]]
getRegions inp = toNestedList $ regionArray $ listArray (bounds guideArray) [1 ..]
  where
    toNestedList :: (IArray a e) => a (Int, Int) e -> [[e]]
    toNestedList arr =
      let bnds = bounds arr
       in [[arr ! (i, j) | j <- [snd (fst bnds) .. snd (snd bnds)]] | i <- [fst (fst bnds) .. fst (snd bnds)]]
    guideArray :: Array _ _
    guideArray = listArray ((0, 0), (length inp - 1, length (head inp) - 1)) (concat inp)
    iWithNbrs = (\i -> (i,) <$> neighbors (bounds guideArray) i) `concatMap` indices guideArray
    iWithRelevantNbrs = filter (\(i, nbs) -> guideArray ! i == guideArray ! nbs) iWithNbrs
    regionArray :: UArray (Int, Int) Int -> UArray (Int, Int) Int
    regionArray start =
      let iWithVals = map (second (start !)) iWithRelevantNbrs
          newArr = accum max start iWithVals
       in if newArr == start then start else regionArray newArr

combineWithNbs :: (a -> a -> a -> a -> a -> b) -> a -> [[a]] -> [[b]]
combineWithNbs combiner edge initg = nrows
  where
    edgerow = replicate (length $ head initg) edge
    nrows = zipWith3 rowCombiner (edgerow : init initg) initg (tail initg ++ [edgerow])
    bumpMid mid = zip3 (edge : init mid) mid (tail mid ++ [edge])
    rowCombiner above mid below = zipWith3 spotCombiner above (bumpMid mid) below
    spotCombiner up (left, spot, right) down = combiner up left spot right down

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc12.in" else head args
  grid <- lines <$> readFile filename
  let regions = getRegions grid
  let regionAreas = M.fromListWith (+) $ concatMap (map (,1 :: Int)) regions
  let fenceFinder up lft midval rgt dn = (midval, length [() | oth <- [up, lft, rgt, dn], oth /= midval])
  let fences = M.fromListWith (+) $ concat $ combineWithNbs fenceFinder 0 regions
  let part1 = sum $ (\(rgnum, area) -> area * M.findWithDefault 0 rgnum fences) <$> M.toList regionAreas
  putStrLn $ "Part 1: " ++ show part1
  let edgeDetector up lft midval rgt dn =
        ( midval,
          (if up /= midval then 1 else (0 :: Int))
            + (if lft /= midval then 2 else 0)
            + (if rgt /= midval then 4 else 0)
            + (if dn /= midval then 8 else 0)
        )
  let taggedEdges = combineWithNbs edgeDetector 0 regions
  let sideFinder up lft midval _ _ =
        let upDiffR = fst up /= fst midval
            lftDiffR = fst lft /= fst midval
            (rgn, mval) = midval
            lval = if lftDiffR then 0 else snd lft
            uval = if upDiffR then 0 else snd up
         in ( rgn,
              (if (mval .&. 1 /= 0) && (lval .&. 1 == 0) then 1 else 0)
                + (if (mval .&. 8 /= 0) && (lval .&. 8 == 0) then 1 else 0)
                + (if (mval .&. 2 /= 0) && (uval .&. 2 == 0) then 1 else 0)
                + (if (mval .&. 4 /= 0) && (uval .&. 4 == 0) then 1 else 0)
            )
  let sides = M.fromListWith (+) (concat $ combineWithNbs sideFinder (0, 0) taggedEdges)
  let part2 = sum $ (\(rgnum, area) -> area * M.findWithDefault 0 rgnum sides) <$> M.toList regionAreas
  putStrLn $ "Part 2: " ++ show part2
