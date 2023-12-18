{-# Language BangPatterns #-}
module Dijkstra where

import Data.List
import qualified Data.Set as S
import qualified Data.PQueue.Min as PM

-- conventions as with heapq (i.e., popH takes smalles out and returns it)
-- HeapH v top bot -- top has at most 1 more than bot
data Heap a
  = EmptyH
  | HeapH a (Heap a) (Heap a)

peekH :: Heap a -> Maybe a
peekH EmptyH = Nothing
peekH (HeapH a _ _) = Just a

popH ::
  Ord a => Heap a -> Maybe (a, Heap a)
popH EmptyH = Nothing
popH (HeapH v top bot) =
  case popH top of
    Nothing -> Just (v, EmptyH)
    Just (a, top') ->
      let (a', bot') = pushPopH bot a
       in Just (v, HeapH a' bot' top')

pushH :: Ord a => Heap a -> a -> Heap a
pushH EmptyH x = HeapH x EmptyH EmptyH
pushH (HeapH v top bot) x
  | v < x = HeapH v (pushH bot x) top
  | otherwise = HeapH x (pushH bot v) top

-- Equivalent to (\a b -> fromJust $ popH $ pushH a b)
pushPopH :: Ord a => Heap a -> a -> (a, Heap a)
pushPopH EmptyH x = (x, EmptyH)
pushPopH old@(HeapH v top bot) x
  | x <= v = (x, old)
  | otherwise =
      case (peekH top, peekH bot) of
        (Nothing, Nothing) -> (v, HeapH x EmptyH EmptyH)
        (Just a, Nothing) ->
          if x <= a
            then (v, HeapH x top bot)
            else (v, HeapH a (HeapH x EmptyH EmptyH) bot)
        (Just a, Just b) ->
          if a < b
            then
              let (v', top') = pushPopH top x
               in (v, HeapH v' top' bot)
            else
              let (v', bot') = pushPopH bot x
               in (v, HeapH v' top bot')
        _ -> error "invalid internal structure"

dijkstraGen ::
  (Ord cost, Ord state) =>
  (state -> [(state, cost)]) ->
  state ->
  cost ->
  (cost -> cost -> cost) ->
  (state -> cost) ->
  (state -> Bool) ->
  Maybe (state, cost)
dijkstraGen txn iState iCost comboFunc estimate accept = go S.empty (pushH EmptyH (iCost, iCost, iState))
 where
  go seen heap = case popH heap of
    Nothing -> Nothing
    Just ((_, cost, val), heap')
      | val `S.member` seen -> go seen heap'
      | accept val -> Just (val, cost)
      | otherwise ->
          let nxts = (\(val', cost') -> let ncost = comboFunc cost cost' in (comboFunc (estimate val') ncost, ncost, val')) <$> txn val
              heap'' = foldl' pushH heap' nxts
           in go (S.insert val seen) heap''

dijkstra :: (Num cost, Ord cost, Ord state) => (state -> [(state, cost)]) -> state -> (state -> Bool) -> Maybe (state, cost)
dijkstra txn iState = dijkstraGen txn iState 0 (+) (const 0)

aStar :: (Num cost, Ord cost, Ord state) => (state -> [(state, cost)]) -> state -> (state -> cost) -> (state -> Bool) -> Maybe (state, cost)
aStar txn iState = dijkstraGen txn iState 0 (+)

dijkstraT :: (Ord cost, Ord state) => (state -> [(state, cost)]) -> state -> cost -> (state -> Bool) -> Maybe (state, cost)
dijkstraT txn iState iCost = dijkstraGen txn iState iCost (const id) (const iCost)


dijkstraGenN' ::
  (Ord cost, Ord state) =>
  (state -> [(state, cost)]) ->
  [(state, cost)] ->
  (cost -> cost -> cost) ->
  (state -> cost) ->
  (state -> Bool) ->
  Maybe (state, cost)
dijkstraGenN' txn seeds comboFunc estimate accept = go S.empty (foldl' (\hp (a, b) -> PM.insert (b, b, a) hp) PM.empty seeds)
 where
  go !seen heap = case PM.minView heap of
    Nothing -> Nothing
    Just ((_, cost, val), heap')
      | val `S.member` seen -> go seen heap'
      | accept val -> Just (val, cost)
      | otherwise ->
          let nxts = (\(val', cost') -> let ncost = comboFunc cost cost' in (comboFunc (estimate val') ncost, ncost, val')) <$> txn val
              heap'' = foldl' (flip PM.insert) heap' nxts
           in go (S.insert val seen) heap''

dijkstraGen' :: (Ord cost, Ord state) => (state -> [(state, cost)]) -> state -> cost -> (cost -> cost -> cost) -> (state -> cost) -> (state -> Bool) -> Maybe (state, cost)
dijkstraGen' txn iState iCost = dijkstraGenN' txn [(iState, iCost)]


dijkstra' :: (Num cost, Ord cost, Ord state) => (state -> [(state, cost)]) -> state -> (state -> Bool) -> Maybe (state, cost)
dijkstra' txn iState = dijkstraGen txn iState 0 (+) (const 0)

aStar' :: (Num cost, Ord cost, Ord state) => (state -> [(state, cost)]) -> state -> (state -> cost) -> (state -> Bool) -> Maybe (state, cost)
aStar' txn iState = dijkstraGen txn iState 0 (+)
