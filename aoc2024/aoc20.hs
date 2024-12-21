import Control.Arrow (Arrow (second))
import Control.Monad (guard)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set qualified as S
import Dijkstra qualified
import System.Environment (getArgs)

add :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add (!x1, !y1) (!x2, !y2) = (x1 + x2, y1 + y2)

getNeighbors :: Map (Int, Int) Char -> Int -> Int -> (Int, Int) -> [((Int, Int), Int)]
getNeighbors mp minCost maxCost spot = do
  xoff <- [-maxCost .. maxCost]
  yoff <- [-maxCost .. maxCost]
  let cost = abs xoff + abs yoff
  guard $ cost >= minCost
  guard $ cost <= maxCost
  let nspot = spot `add` (xoff, yoff)
  guard $ '#' /= M.findWithDefault '#' nspot mp
  pure (nspot, cost)

minCostMap :: forall state. (Ord state) => (state -> [(state, Int)]) -> state -> Map state Int
minCostMap txn start = go (M.singleton start 0) (txn start)
  where
    go :: Map state Int -> [(state, Int)] -> Map state Int
    go mp [] = mp
    go mp xs =
      let changes = filter (\(s, c) -> maybe True (c <) (M.lookup s mp)) xs
          getNext (s, c) = second (+ c) <$> txn s
          nexts = concatMap getNext changes
       in go (M.fromList changes `M.union` mp) nexts

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc20.in" else head args
  grid <- lines <$> readFile filename
  let gridMap = M.fromList $ do
        (xidx, row) <- zip [0 ..] grid
        (yidx, ch) <- zip [0 ..] row
        pure ((xidx, yidx), ch)
  let boundNbrs = getNeighbors gridMap 1 1
  let startSpot = fst $ head $ filter ((== 'S') . snd) $ M.toList gridMap
  let accept = (== 'E') . (gridMap !)
  (baseDist, endSpot) <- case Dijkstra.dijkstra boundNbrs startSpot accept of
    Nothing -> ioError (userError "No path!")
    Just (e, t) -> pure (t, e)

  let distFromStart = minCostMap boundNbrs startSpot
      distFromEnd = minCostMap boundNbrs endSpot
  let findCheatNbrs = getNeighbors gridMap 2 2
  let distCheating startd (dest, destd) = startd + destd + distFromEnd ! dest
  putStr "Part 1: "
  print $
    length $
      concatMap
        ( filter (<= baseDist - 100)
            . ( \(k, v) ->
                  map (v `distCheating`) (findCheatNbrs k)
              )
        )
        (M.toList distFromStart)
  let findCheatNbrs' = getNeighbors gridMap 2 20
  putStr "Part 2: "
  print $
    length $
      concatMap
        ( filter (<= baseDist - 100)
            . ( \(k, v) ->
                  map (v `distCheating`) (findCheatNbrs' k)
              )
        )
        (M.toList distFromStart)