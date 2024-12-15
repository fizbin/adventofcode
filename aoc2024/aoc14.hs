{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Arrow (Arrow (second))
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Char (isDigit)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import System.Environment (getArgs)

data Robot = Robot Int Int Int Int deriving (Show)

width, height :: Int
width = 101
height = 103

findints :: String -> [Int]
findints "" = []
findints ('-' : x : xs) | isDigit x = case findints (x : xs) of
  [] -> error "Can't happen"
  (n : ns) -> (-n) : ns
findints (x : xs) | not (isDigit x) = findints xs
findints s = let (n, s') = head (reads s) in n : findints s'

parseRobot :: String -> Robot
parseRobot robotStr = case findints robotStr of
  [x, y, vx, vy] -> Robot x y vx vy
  _ -> error $ "Bad parse: " ++ robotStr

spotAt :: Int -> Robot -> (Int, Int)
spotAt gen (Robot x y vx vy) = ((x + gen * vx) `mod` width, (y + gen * vy) `mod` height)

neighbors :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [(Int, Int)]
neighbors bds spot =
  filter
    (inRange bds)
    [ (fst spot - 1, snd spot),
      (fst spot + 1, snd spot),
      (fst spot, snd spot - 1),
      (fst spot, snd spot + 1)
    ]

toNestedList :: (IArray a e) => a (Int, Int) e -> [[e]]
toNestedList arr =
  let bnds = bounds arr
   in [[arr ! (i, j) | j <- [snd (fst bnds) .. snd (snd bnds)]] | i <- [fst (fst bnds) .. fst (snd bnds)]]

getRegions :: [[Bool]] -> [[Maybe Int]]
getRegions inp = toNestedList $ regionArray initialPartition (indices guideArray)
  where
    initialPartition =
      let arange = listArray (bounds guideArray) [1 ..] :: UArray (Int, Int) Int
          genArray (l, u) f = listArray (l, u) $ map f $ range (l, u)
       in genArray (bounds guideArray) (\idx -> if guideArray ! idx then Just (arange ! idx) else Nothing)
    guideArray :: Array _ _
    guideArray = listArray ((0, 0), (length inp - 1, length (head inp) - 1)) (concat inp)
    regionArray :: Array (Int, Int) (Maybe Int) -> [(Int, Int)] -> Array (Int, Int) (Maybe Int)
    regionArray !start [] = start
    regionArray !start workList =
      let iWithNbrs = (\i -> (i,) <$> neighbors (bounds guideArray) i) `concatMap` filter (guideArray !) workList
          iWithRelevantNbrs = filter (\(_, nbs) -> guideArray ! nbs) iWithNbrs
          iWithVals = map (second (start !)) iWithRelevantNbrs
          iNec = filter (\(i, nval) -> nval > (start ! i)) iWithVals
          newArr = accum max start iNec
       in regionArray newArr (S.toList . S.fromList $ concatMap (neighbors (bounds guideArray) . fst) iNec)

scoreGeneration :: Int -> [Robot] -> Integer
scoreGeneration gen robots =
  let spots = spotAt gen <$> robots
      blankArray :: UArray _ _
      blankArray = listArray ((0, 0), (width - 1, height - 1)) (repeat False)
      visitedArray = blankArray // ((,True) <$> spots)
      regions = getRegions (toNestedList visitedArray)
      areaMap = M.fromListWith (+) $ concatMap (mapMaybe ((,1) <$>)) regions
   in sum $ map (\x -> x * x) $ M.elems areaMap

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc14.in" else head args
  robotSpecs <- lines <$> readFile filename
  let robots = parseRobot <$> robotSpecs
  let spot100 = spotAt 100 <$> robots
  let quadrants =
        filter (\(a, b) -> (a /= EQ) && (b /= EQ)) $
          (\(a, b) -> (compare a (width `div` 2), compare b (height `div` 2))) <$> spot100
  let quadrantcount = M.elems $ M.fromListWith (+) $ (,1 :: Int) <$> quadrants
  putStrLn $ "Part 1: " ++ show (product quadrantcount)
  score <- forM [0 .. width * height] $ \gen -> do
    let x = scoreGeneration gen robots
    x' <- evaluate x
    -- putStrLn $ "finished gen " ++ show gen
    pure (x', gen)
  let best = maximum score
  putStrLn $ "Part 2: " ++ show (snd best)