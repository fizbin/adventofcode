{-# LANGUAGE BangPatterns #-}

module Dijkstra
  ( dijkstraGenN',
    dijkstraGen',
    dijkstra',
    aStar',
    dijkstraGenN,
    dijkstraGen,
    aStar,
    dijkstra,
  )
where

import Data.IntMap.Strict qualified as IM
import Data.List
import Data.PQueue.Min qualified as PM
import Data.Set qualified as S

dijkstraGenN' ::
  (Ord state) =>
  (state -> [(state, Int)]) ->
  [(state, Int)] ->
  (Int -> Int -> Int) ->
  (state -> Int) ->
  (state -> Bool) ->
  Maybe (state, Int)
dijkstraGenN' txn seeds comboFunc estimate accept = go S.empty (foldl' (\hp (a, b) -> IM.insertWith (++) b [(b, a)] hp) IM.empty seeds)
  where
    go !seen heap = case IM.minViewWithKey heap of
      Nothing -> Nothing
      Just ((minPrio, minList), hv) ->
        let (cost, val, heap') = case minList of
              [] -> error "Empty heap slot"
              [(c, v)] -> (c, v, hv)
              ((c, v) : rst) -> (c, v, IM.insert minPrio rst heap)
         in if val `S.member` seen
              then go seen heap'
              else
                if accept val
                  then Just (val, cost)
                  else
                    let nxts = (\(val', cost') -> let ncost = comboFunc cost cost' in (comboFunc (estimate val') ncost, (ncost, val'))) <$> txn val
                        ifn ent Nothing = Just [ent]
                        ifn ent (Just lst) = Just (ent : lst)
                        heap'' = foldl' (\hp (key, ent) -> IM.alter (ifn ent) key hp) heap' nxts
                     in go (S.insert val seen) heap''

dijkstraGen' :: (Ord state) => (state -> [(state, Int)]) -> state -> Int -> (Int -> Int -> Int) -> (state -> Int) -> (state -> Bool) -> Maybe (state, Int)
dijkstraGen' txn iState iCost = dijkstraGenN' txn [(iState, iCost)]

dijkstra' :: (Ord state) => (state -> [(state, Int)]) -> state -> (state -> Bool) -> Maybe (state, Int)
dijkstra' txn iState = dijkstraGen' txn iState 0 (+) (const 0)

aStar' :: (Ord state) => (state -> [(state, Int)]) -> state -> (state -> Int) -> (state -> Bool) -> Maybe (state, Int)
aStar' txn iState = dijkstraGen' txn iState 0 (+)

dijkstraGenN ::
  (Ord cost, Ord state) =>
  (state -> [(state, cost)]) ->
  [(state, cost)] ->
  (cost -> cost -> cost) ->
  (state -> cost) ->
  (state -> Bool) ->
  Maybe (state, cost)
dijkstraGenN txn seeds comboFunc estimate accept = go S.empty (foldl' (\hp (a, b) -> PM.insert (b, b, a) hp) PM.empty seeds)
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

dijkstraGen :: (Ord cost, Ord state) => (state -> [(state, cost)]) -> state -> cost -> (cost -> cost -> cost) -> (state -> cost) -> (state -> Bool) -> Maybe (state, cost)
dijkstraGen txn iState iCost = dijkstraGenN txn [(iState, iCost)]

dijkstra :: (Num cost, Ord cost, Ord state) => (state -> [(state, cost)]) -> state -> (state -> Bool) -> Maybe (state, cost)
dijkstra txn iState = dijkstraGen txn iState 0 (+) (const 0)

aStar :: (Num cost, Ord cost, Ord state) => (state -> [(state, cost)]) -> state -> (state -> cost) -> (state -> Bool) -> Maybe (state, cost)
aStar txn iState = dijkstraGen txn iState 0 (+)
