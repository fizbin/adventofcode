-- stack --resolver lts-18.18 script --package pqueue --package containers
{-# OPTIONS_GHC -Wall -Wno-unused-top-binds #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.Maybe

import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.PQueue.Min as PQ
import qualified Data.Set as S
import System.Console.GetOpt

--import Debug.Trace
import System.Environment

-- HeapH isBalanced v top bot -- top has at most 1 more than bot
data Heap a
  = EmptyH
  | HeapH Bool a (Heap a) (Heap a)

peekH :: Heap a -> Maybe a
peekH EmptyH = Nothing
peekH (HeapH _ a _ _) = Just a

-- This is done so that popH and popH' can have nearly identical code,
-- except for whether they refer to pushPopH or pushPopH'
popHTemplate ::
     Ord a => (Heap a -> a -> (a, Heap a)) -> Heap a -> Maybe (a, Heap a)
popHTemplate _ EmptyH = Nothing
popHTemplate ppf (HeapH True v top bot) =
  case (peekH top, peekH bot) of
    (Nothing, Nothing) -> Just (v, EmptyH)
    (Just a, Just b) ->
      if a < b
        then (v, ) . (\(v', hp) -> HeapH False v' bot hp) <$>
             popHTemplate ppf top
        else (v, ) . (\(v', hp) -> HeapH False v' top hp) <$>
             popHTemplate ppf bot
    _ -> error "invalid internal structure"
popHTemplate ppf (HeapH False v top bot) =
  case popH top of
    Nothing -> error "invalid internal structure"
    Just (a, top') ->
      let (a', bot') = ppf bot a
       in Just (v, HeapH True a' top' bot')

popH :: Ord a => Heap a -> Maybe (a, Heap a)
popH = popHTemplate pushPopH

pushH :: Ord a => Heap a -> a -> Heap a
pushH EmptyH x = HeapH True x EmptyH EmptyH
pushH (HeapH isBal v top bot) x
  | v < x = HeapH (not isBal) v (pushH bot x) top
  | otherwise = HeapH (not isBal) x (pushH bot v) top

-- Equivalent to (\a b -> fromJust $ popH $ pushH a b)
pushPopH :: Ord a => Heap a -> a -> (a, Heap a)
pushPopH EmptyH x = (x, EmptyH)
pushPopH old@(HeapH isBal v top bot) x
  | x <= v = (x, old)
  | otherwise =
    case (peekH top, peekH bot) of
      (Nothing, Nothing) -> (v, HeapH True x EmptyH EmptyH)
      (Just a, Nothing) ->
        if x <= a
          then (v, HeapH False x top bot)
          else (v, HeapH False a (HeapH True x EmptyH EmptyH) bot)
      (Just a, Just b) ->
        if a < b
          then let (v', top') = pushPopH top x
                in (v, HeapH isBal v' top' bot)
          else let (v', bot') = pushPopH bot x
                in (v, HeapH isBal v' top bot')
      _ -> error "invalid internal structure"

-- bad handrolled heap implementations:
-- popH' is just popH, except that it calls pushPopH' in one case
popH' :: Ord a => Heap a -> Maybe (a, Heap a)
popH' = popHTemplate pushPopH'

-- This is the implementation that's just a bad idea
pushPopH' :: Ord a => Heap a -> a -> (a, Heap a)
pushPopH' EmptyH x = (x, EmptyH)
pushPopH' (HeapH isBal v top bot) x =
  if v < x
    then let (x', top') = pushPopH' top x
             (x'', bot') = pushPopH' bot x'
          in (v, HeapH isBal x'' top' bot')
    else let (v', top') = pushPopH' top v     -- This bit is such a bad idea
             (v'', bot') = pushPopH' bot v'   -- seriously, once I realized
          in (x, HeapH isBal v'' top' bot')   -- what it implied... of course
                                              -- it was slow. So, so slow.
    -- the answer is that the "else" clause should be simply
    --     else (x, Heap isBal v top bot)
    -- or bettter yet:
    --     else (x, old)
    -- where "old" is a pattern bound to (HeapH isBal v top bot) on the
    -- top line. See the definition of pushPopH above.

instance (Ord a) => Semigroup (Heap a) where
  v <> EmptyH = v
  EmptyH <> v = v
  (HeapH bal1 v1 top1 bot1) <> (HeapH bal2 v2 top2 bot2) =
    if bal1 == bal2
      then flip pushH v2 $ HeapH True v1 (top1 <> bot2) (bot1 <> top2)
      else flip pushH v2 $ HeapH False v1 (top1 <> top2) (bot1 <> bot2)

instance (Ord a) => Monoid (Heap a) where
  mempty = EmptyH

data ListWithResort =
  ListWithResort

data ListWithSortedMerge =
  ListWithSortedMerge

data HandrolledQueue =
  HandrolledQueue

data PQQueue =
  PQQueue

data BadHandrolledQueue =
  BadHandrolledQueue

class QueueStrategy strat where
  type QueueType strat :: * -> *
  initQ :: (Ord a) => strat -> a -> QueueType strat a
  minViewQ ::
       (Ord a) => strat -> QueueType strat a -> Maybe (a, QueueType strat a)
  insertQ :: (Ord a) => strat -> QueueType strat a -> [a] -> QueueType strat a

instance QueueStrategy HandrolledQueue where
  type QueueType HandrolledQueue = Heap
  initQ _ initVal = pushH mempty initVal
  minViewQ _ = popH
  insertQ _ q = foldl' pushH q

instance QueueStrategy BadHandrolledQueue where
  type QueueType BadHandrolledQueue = Heap
  initQ _ initVal = pushH mempty initVal
  minViewQ _ = popH'
  insertQ _ q = foldl' pushH q

instance QueueStrategy PQQueue where
  type QueueType PQQueue = PQ.MinQueue
  initQ _ initVal = PQ.insert initVal mempty
  minViewQ _ = PQ.minView
  insertQ _ q = foldl' (flip PQ.insert) q

instance QueueStrategy ListWithResort where
  type QueueType ListWithResort = []
  initQ _ initVal = [initVal]
  minViewQ _ [] = Nothing
  minViewQ _ (a:as) = Just (a, as)
  insertQ _ q vals = sort $ q ++ vals

instance QueueStrategy ListWithSortedMerge where
  type QueueType ListWithSortedMerge = []
  initQ _ initVal = [initVal]
  minViewQ _ [] = Nothing
  minViewQ _ (a:as) = Just (a, as)
  insertQ _ q = mergeSorted q . sort

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

-- takes 5m18s (Ah!) on both parts
p1 ::
     forall s p. (QueueStrategy s, Ord p)
  => s
  -> Map p [p]
  -> Map p Int
  -> p
  -> p
  -> Int
p1 strategy nbmap dataMap start goal = go mempty (initQ strategy (0, start))
  where
    go visited theap =
      case minViewQ strategy theap of
        Nothing -> error "Ran out of heap"
        Just ((cost, spot), heap') ->
          if S.member spot visited
            then go visited heap'
            else if spot == goal
                   then cost
                   else go (S.insert spot visited) $
                        insertQ strategy heap' $
                        map (\nb -> (cost + dataMap ! nb, nb)) (nbmap ! spot)

processOpts ::
     Ord p => [String] -> IO ([String], Map p [p] -> Map p Int -> p -> p -> Int)
processOpts argv =
  case getOpt Permute options argv of
    (flags, nonFlags, []) ->
      getDoitFunc (listToMaybe flags) >>= \f -> pure (nonFlags, f)
    (_, _, errs) -> ioError (userError (concat errs))
  where
    options = [Option "s" ["strategy"] (ReqArg id "STRATEGY") "Strategy"]
    getDoitFunc Nothing = pure (p1 PQQueue)
    getDoitFunc (Just "ListWithResort") = pure (p1 ListWithResort)
    getDoitFunc (Just "ListWithSortedMerge") = pure (p1 ListWithSortedMerge)
    getDoitFunc (Just "HandrolledQueue") = pure (p1 HandrolledQueue)
    getDoitFunc (Just "PQQueue") = pure (p1 PQQueue)
    getDoitFunc (Just "BadHandrolledQueue") = pure (p1 BadHandrolledQueue)
    getDoitFunc _ =
      ioError
        (userError $
         "unrecognized strategy; recognized: " ++
         "ListWithResort, ListWithSortedMerge, HandrolledQueue, PQQueue, BadHandrolledQueue")

main :: IO ()
main = do
  args1 <- getArgs
  (args, doitFunc) <- processOpts args1
  let filename =
        if null args
          then "aoc15.in"
          else head args
  datas <- map (map (read . (: []))) . words <$> readFile filename
  let coordsWithNeighbors =
        mkCoordsWithNeighbors (length datas) (length $ head datas)
  let allspots = concat datas
  let datamap = M.fromList $ zip (map fst coordsWithNeighbors) allspots
  let nbmap = M.fromList coordsWithNeighbors
  print $
    doitFunc nbmap datamap (0, 0) (length datas - 1, length (head datas) - 1)
  let incrow = map ((+ 1) . (`mod` 9))
  let bigdata1 = map (concat . take 5 . iterate incrow) datas :: [[Int]]
  let bigdata = concat $ take 5 $ iterate (map incrow) bigdata1
  let bigCwNbs = mkCoordsWithNeighbors (length bigdata) (length $ head bigdata)
  let bigdatamap = M.fromList $ zip (map fst bigCwNbs) (concat bigdata)
  let bigNbmap = M.fromList bigCwNbs
  print $
    doitFunc
      bigNbmap
      bigdatamap
      (0, 0)
      (length bigdata - 1, length (head bigdata) - 1)
