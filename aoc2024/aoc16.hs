import Control.Arrow (Arrow (second, first))
import Control.Monad (guard)
import Data.Map (Map, (!))
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Dijkstra qualified
import System.Environment (getArgs)

add :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add (!x1, !y1) (!x2, !y2) = (x1 + x2, y1 + y2)

getNeighbors :: Map (Int, Int) Char -> ((Int, Int), (Int, Int)) -> [(((Int, Int), (Int, Int)), Int)]
getNeighbors mp (spot, dir) = do
  (nspot, ndir, cost) <- [(spot `add` dir, dir, 1), (spot, turnLeft dir, 1000), (spot, turnRight dir, 1000)]
  guard $ '#' /= M.findWithDefault '#' nspot mp
  pure ((nspot, ndir), cost)
  where
    turnLeft (a, b) = (-b, a)
    turnRight (a, b) = (b, -a)

getNeighborsBackwards :: Map (Int, Int) Char -> ((Int, Int), (Int, Int)) -> [(((Int, Int), (Int, Int)), Int)]
getNeighborsBackwards mp (spot, dir) = first (second bkwd) <$> getNeighbors mp (spot, bkwd dir)
    where
        bkwd (x, y) = (-x, -y)

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

walkMinCost :: forall state. (Show state, Ord state) => (state -> [(state, Int)]) -> state -> Map state Int -> S.Set state
walkMinCost backwardsTxn endpos minCost = S.fromList $ go [(endpos, minCost ! endpos)]
 where
    go [] = []
    go ws = let valid = filter (\(s,c) -> Just c == M.lookup s minCost) ws
                getNext (s, c) = second (c -) <$> backwardsTxn s
                nexts = concatMap getNext valid
             in (fst <$> valid) ++ go nexts

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc16.in" else head args
  grid <- lines <$> readFile filename
  let gridMap = M.fromList $ do
        (xidx, row) <- zip [0 ..] grid
        (yidx, ch) <- zip [0 ..] row
        pure ((xidx, yidx), ch)
  let boundNbrs = getNeighbors gridMap
  let startSpot = fst $ head $ filter ((== 'S') . snd) $ M.toList gridMap
  let accept = (== 'E') . (gridMap !) . fst
  (part1, endspot) <- case Dijkstra.dijkstra boundNbrs (startSpot, (0, 1)) accept of
    Nothing -> ioError (userError "No path!")
    Just (e, t) -> pure (t, e)
  print part1
  let minMap = minCostMap boundNbrs (startSpot, (0, 1))
      minSet = walkMinCost (getNeighborsBackwards gridMap) endspot minMap
  print (S.size $ S.map fst minSet)
  