-- stack --resolver nightly-2021-11-28 script --package mtl --package containers
{-# OPTIONS_GHC -Wall -Wno-unused-top-binds #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TupleSections #-}

import Data.List

import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.Set as S

--import Debug.Trace
import System.Environment

-- HeapH isBalanced v top bot -- top has at most 1 more than bot
data Heap a
  = EmptyH
  | HeapH Bool a (Heap a) (Heap a)

popH :: Ord a => Heap a -> Maybe (a, Heap a)
popH EmptyH = Nothing
popH (HeapH iBal v top bot) =
  case popH top of
    Nothing -> Just (v, EmptyH)
    Just (a, top') ->
      let (b, bot') = pushPopH bot a
       in Just (v, HeapH (not iBal) b bot' top')

pushH :: Ord a => Heap a -> a -> Heap a
pushH EmptyH x = HeapH True x EmptyH EmptyH
pushH (HeapH iBal v top bot) x
  | v < x = HeapH (not iBal) v (pushH bot x) top
  | otherwise = HeapH (not iBal) x (pushH bot v) top

-- Equivalent to (\a b -> fromJust $ popH $ pushH a b)
pushPopH :: Ord a => Heap a -> a -> (a, Heap a)
pushPopH EmptyH x = (x, EmptyH)
pushPopH (HeapH iBal v top bot) x =
  if v < x
    then let (x', top') = pushPopH top x
             (x'', bot') = pushPopH bot x'
          in (v, HeapH iBal x'' top' bot')
    else let (v', top') = pushPopH top v
             (v'', bot') = pushPopH bot v'
          in (x, HeapH iBal v'' top' bot')

instance (Ord a) => Semigroup (Heap a) where
  v <> EmptyH = v
  EmptyH <> v = v
  (HeapH bal1 v1 top1 bot1) <> (HeapH bal2 v2 top2 bot2) =
    if bal1 == bal2
      then flip pushH v2 $ HeapH True v1 (top1 <> bot2) (bot1 <> top2)
      else flip pushH v2 $ HeapH False v1 (top1 <> top2) (bot1 <> bot2)

instance (Ord a) => Monoid (Heap a) where
  mempty = EmptyH

mkCoordsWithNeighbors :: Int -> Int -> [((Int, Int), [(Int, Int)])]
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

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted [] x = x
mergeSorted x [] = x
mergeSorted a1@(a:as) b1@(b:bs) =
  if a < b
    then a : mergeSorted as b1
    else b : mergeSorted a1 bs

p1 :: (Ord a, Ord p, Num a) => Map p [p] -> Map p a -> p -> p -> a
p1 nbmap dataMap start goal = go mempty (pushH mempty (0, start))
  where
    go visited theap =
      case popH theap of
        Nothing -> error "Ran out of heap"
        Just ((cost, spot), heap') ->
          if S.member spot visited
            then go visited heap'
            else if spot == goal
                   then cost
                   else go (S.insert spot visited) $
                        foldl'
                          (\heap nb -> pushH heap (cost + dataMap ! nb, nb))
                          heap'
                          (nbmap ! spot)

p1' :: Ord p => Map p [p] -> Map p Int -> p -> p -> Int
p1' nbmap dataMap start goal = go mempty [(0, start)]
  where
    go visited theap =
      case theap of
        [] -> error "Ran out of heap"
        ((cost, spot):heap') ->
          if S.member spot visited
            then go visited heap'
            else if spot == goal
                   then cost
                   else go (S.insert spot visited) $
                        sort $ heap' ++
                        map (\nb -> (cost + dataMap ! nb, nb)) (nbmap ! spot)

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc15.in"
          else head args
  datas <- map (map (read . (: []))) . words <$> readFile filename :: IO [[Int]]
  let coordsWithNeighbors =
        mkCoordsWithNeighbors (length datas) (length $ head datas)
  let allspots = concat datas
  let datamap = M.fromList $ zip (map fst coordsWithNeighbors) allspots
  let nbmap = M.fromList coordsWithNeighbors
  print $ p1' nbmap datamap (0, 0) (length datas - 1, length (head datas) - 1)
  let incrow = map ((+ 1) . (`mod` 9))
  let bigdata1 = map (concat . take 5 . iterate incrow) datas :: [[Int]]
  let bigdata = concat $ take 5 $ iterate (map incrow) bigdata1
  let bigCwNbs = mkCoordsWithNeighbors (length bigdata) (length $ head bigdata)
  let bigdatamap = M.fromList $ zip (map fst bigCwNbs) (concat bigdata)
  let bigNbmap = M.fromList bigCwNbs
  print $
    p1'
      bigNbmap
      bigdatamap
      (0, 0)
      (length bigdata - 1, length (head bigdata) - 1)
