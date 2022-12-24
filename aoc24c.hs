{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

{- This will work on the full problem input, but will take three to four times as -}
{- long as aoc24.hs, which is based on an A* search. -}

import Control.Monad ( guard, when )
import Data.List (sortBy)
import qualified Data.Map as M
import System.Environment (getArgs)

-- import Debug.Trace (trace)

neigh :: Int -> Int -> M.Map (Int, Int) Char -> ((Int, Int), Int) -> [((Int, Int), Int)]
neigh pitchHeight pitchWidth grid ((row, col), t) = do
  (row', col') <- [(row + r, col + c) | (r, c) <- [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)]]
  guard $ '#' /= M.findWithDefault '#' (row, col) grid
  when ((1 <= row) && (row <= pitchHeight)) $ do
    guard $ '<' /= grid M.! (row', 1 + ((col' + t) `mod` pitchWidth))
    guard $ '>' /= grid M.! (row', 1 + ((col' - t - 2) `mod` pitchWidth))
    guard $ '^' /= grid M.! (1 + ((row' + t) `mod` pitchHeight), col')
    guard $ 'v' /= grid M.! (1 + ((row' - t - 2) `mod` pitchHeight), col')
  pure ((row', col'), t + 1)

estimate :: (Int, Int) -> ((Int, Int), a) -> Int
estimate goal ((row, col), _) = abs (fst goal - row) + abs (snd goal - col)

-- Apply technique mentioned in https://www.reddit.com/r/haskell/comments/p2tm2n/comment/h8muqzd/,
-- only modified slightly as all steps are size 1

data Worky a = Done a | Worky (Worky a) | Fail deriving (Show, Eq)

instance Semigroup (Worky a) where
  (Done x) <> _ = Done x
  _ <> (Done x) = Done x
  Fail <> x = x
  x <> Fail = x
  (Worky a) <> (Worky b) = Worky (a <> b)

instance Monoid (Worky a) where
  mempty = Fail

finish :: Worky a -> Maybe a
finish (Done x) = Just x
finish (Worky w) = finish w
finish Fail = Nothing

type State = ((Int, Int), Int)
data CAFShape = CAFShape (Worky State) CAFShape CAFShape CAFShape

workGoal :: (State -> [State]) -> (State -> Int) -> (State -> Bool) -> State -> Worky State
workGoal nbF estF goalF = go
 where
  mkGoCAF ((r, c), t) = CAFShape (go ((r, c), t)) (mkGoCAF ((r, c), t+1)) (mkGoCAF ((r+1, c), t)) (mkGoCAF ((r, c+1), t))
  goCAF = mkGoCAF ((0, 0), 0)
  findInCAF (CAFShape a _ _ _) ((0, 0), 0) = a
  findInCAF (CAFShape _ gt gr gc) ((r, c), t)
    | (r < 0) || (c < 0) || (t < 0) = Fail
    | t > 0 = findInCAF gt ((r, c), t-1)
    | r > 0 = findInCAF gr ((r-1, c), t)
    | c > 0 = findInCAF gc ((r, c-1), t)
    | otherwise = error ("What? " ++ show ((r,c),t))
  go iState =
    if goalF iState
      then Done iState
      else
        let nbs = sortBy (\a b -> compare (estF a) (estF b) <> compare a b) (nbF iState)
         in Worky $ foldMap (findInCAF goCAF) nbs

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc24.in"
          else head args
  s <- lines <$> readFile filename
  let grid = M.fromList [((row, col), ch) | (row, line) <- zip [0 ..] s, (col, ch) <- zip [0 ..] line]
  let start = head $ map ((0,) . fst) $ filter (\(_, ch) -> ch == '.') $ zip [0 ..] (head s)
  let goal = head $ map ((length s - 1,) . fst) $ filter (\(_, ch) -> ch == '.') $ zip [0 ..] (last s)
  let pitchWidth = length (head s) - 2
  let pitchHeight = length s - 2

  let Just part1 = finish $ workGoal (neigh pitchHeight pitchWidth grid) (estimate goal) ((== goal) . fst) (start, 0)
  print $ snd part1
  let Just part2a = finish $ workGoal (neigh pitchHeight pitchWidth grid) (estimate start) ((== start) . fst) part1
  let Just part2b = finish $ workGoal (neigh pitchHeight pitchWidth grid) (estimate goal) ((== goal) . fst) part2a
  print $ snd part2b
