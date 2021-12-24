-- stack --resolver lts-18.18 script --package multiset --package containers
{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

--import Debug.Trace
import Data.Char
import Data.List
import System.Environment
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
-- HeapH isBalanced v top bot -- top has at most 1 more than bot
data Heap a
  = EmptyH
  | HeapH Bool a (Heap a) (Heap a)

peekH :: Heap a -> Maybe a
peekH EmptyH = Nothing
peekH (HeapH _ a _ _) = Just a

popH ::
     Ord a => Heap a -> Maybe (a, Heap a)
popH EmptyH = Nothing
popH (HeapH True v top bot) =
  case (peekH top, peekH bot) of
    (Nothing, Nothing) -> Just (v, EmptyH)
    (Just a, Just b) ->
      if a < b
        then (v, ) . (\(v', hp) -> HeapH False v' bot hp) <$>
             popH top
        else (v, ) . (\(v', hp) -> HeapH False v' top hp) <$>
             popH bot
    _ -> error "invalid internal structure"
popH (HeapH False v top bot) =
  case popH top of
    Nothing -> error "invalid internal structure"
    Just (a, top') ->
      let (a', bot') = pushPopH bot a
       in Just (v, HeapH True a' top' bot')

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


acost :: Num p => Char -> p
acost 'A' = 1
acost 'B' = 10
acost 'C' = 100
acost 'D' = 1000
acost _ = error "Bad amphipod"

roomFor :: Num p => Char -> p
roomFor 'A' = 0
roomFor 'B' = 1
roomFor 'C' = 2
roomFor 'D' = 3
roomFor _ = error "Bad amphipod"


dijkstra :: (Num cost, Ord cost, Ord state) => (state -> [(state, cost)]) -> state -> (state -> Bool) -> Maybe (state, cost)
dijkstra txn iState accept = go S.empty (pushH EmptyH (0, iState))
  where
    go seen heap = case popH heap of
        Nothing -> Nothing
        Just ((cost, val), heap')
            | val `S.member` seen -> go seen heap'
            | accept val -> Just (val, cost)
            | otherwise -> let nxts = (\(val', cost') -> (cost + cost', val')) <$> txn val
                               heap'' = foldl' pushH heap' nxts
                            in go (S.insert val seen) heap''

data BurrowState = BurrowState {
    hall :: String, rooms :: [String]
} deriving (Eq, Show, Ord)

listRep :: [a] -> Int -> a -> [a]
listRep (_:ss) 0 a = a:ss
listRep [] _ _ = error "listRep out of room"
listRep (s:ss) i a = s:listRep ss (i-1) a

nextMoves :: Int -> BurrowState -> [(BurrowState, Int)]
nextMoves roomSize BurrowState{..} = outOfRooms ++ inToRooms
  where
      range x y = if x < y then [x..y] else [y..x]
      doorFor room = 2 + 2*room
      outOfRooms = do
          dest <- [0,1,3,5,7,9,10]
          guard (hall !! dest == '.')
          room <- [0,1,2,3]
          case uncons (rooms !! room) of
            Nothing -> empty
            Just (amphi, room') -> do
                guard (not $ null (rooms !! room))
                guard (all ((=='.') . (hall !!)) $ range dest (doorFor room))
                let fromLintel = length $ range dest (doorFor room)
                let tDist = fromLintel + roomSize - length (rooms !! room)
                let hall' = listRep hall dest amphi
                let rooms' = listRep rooms room room'
                pure (BurrowState hall' rooms', acost amphi * tDist)
      inToRooms = do
          start <- [0,1,3,5,7,9,10]
          let amphi = hall !! start
          guard (amphi /= '.')
          let destRoom = roomFor amphi
          let dest = doorFor destRoom
          guard (all (\i -> (i == start) || (hall !! i == '.')) $ range dest start)
          let room = rooms !! destRoom
          guard (all (== amphi) $ rooms !! destRoom)
          let hall' = listRep hall start '.'
          let rooms' = listRep rooms destRoom (amphi:room)
          let tDist = length (range dest start) + roomSize - length room - 1
          pure (BurrowState hall' rooms', tDist * acost amphi)

main :: IO ()
main = do
  args <- getArgs
  let filename = maybe "aoc23.in" fst $ uncons args
  datas <- lines <$> readFile filename
  let toproomcontents = filter isLetter (datas !! 2)
  let botroomcontents = filter isLetter (datas !! 3)
  let irooms = zipWith (\a b -> [a, b]) toproomcontents botroomcontents
  let ihall = filter (== '.') (datas !! 1)
  let finalState = BurrowState ihall ["AA", "BB", "CC", "DD"]
  print $ snd <$> dijkstra (nextMoves 2) (BurrowState ihall irooms) (== finalState)
  let finalState' = BurrowState ihall ["AAAA", "BBBB", "CCCC", "DDDD"]
  let shove a b = head a : b ++ tail a
  let irooms' = zipWith shove irooms ["DD", "CB", "BA", "AC"]
  print $ snd <$> dijkstra (nextMoves 4) (BurrowState ihall irooms') (== finalState')