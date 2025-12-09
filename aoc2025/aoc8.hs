import Data.List (sortBy, sortOn)
import Data.Map.Strict qualified as M
import Data.Ord (comparing)
import Data.Ord qualified
import Data.Set qualified as S
import System.Environment (getArgs)

distSq :: (Int, Int, Int) -> (Int, Int, Int) -> Int
distSq (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ t + (y1 - y2) ^ t + (z1 - z2) ^ t
  where
    t = 2 :: Int

findGridNums :: M.Map Int Int -> M.Map Int (S.Set Int) -> M.Map Int Int
findGridNums oldGrid connected = go $ M.toList oldGrid
  where
    go [] = oldGrid
    go (pair : pairs) =
      let (x, xGridNum) = pair
          nbsGridNum = S.map (flip (M.findWithDefault xGridNum) oldGrid) (M.findWithDefault S.empty x connected)
       in case S.lookupMin nbsGridNum of
            Nothing -> go pairs
            Just n | n >= xGridNum -> go pairs
            Just n -> findGridNums (M.insert x n oldGrid) connected

-- import Debug.Trace
part1 :: [(Int, Int, Int)] -> Int
part1 dataPoints = product largestThree
  where
    allPairs0 = [(x, y) | x <- [0 .. length dataPoints - 2], y <- [x + 1 .. length dataPoints - 1]]
    allPairs = sortOn (\(x, y) -> distSq (dataPoints !! x) (dataPoints !! y)) allPairs0
    connectedPairs = take 1000 allPairs
    connectedGr = M.fromListWith S.union $ do
      (x, y) <- connectedPairs
      [(x, S.singleton y), (y, S.singleton x)]
    gridNums0 = M.fromList [(x, x + 1) | x <- [0 .. length dataPoints - 1]]
    gridNums = findGridNums gridNums0 connectedGr
    sizeMap = M.fromListWith (+) $ [(y, 1 :: Int) | (_, y) <- M.toList gridNums]
    largestThree = take 3 $ sortBy (comparing Data.Ord.Down) (M.elems sizeMap)

part2 :: [(Int, Int, Int)] -> Int
part2 dataPoints = let (idx1, idx2) = lastNeededPair gridNums0 connectedGr0 remainingConn
                       (x1, _, _) = dataPoints !! idx1
                       (x2, _, _) = dataPoints !! idx2
                   in x1*x2
  where
    allPairs0 = [(x, y) | x <- [0 .. length dataPoints - 2], y <- [x + 1 .. length dataPoints - 1]]
    allPairs = sortOn (\(x, y) -> distSq (dataPoints !! x) (dataPoints !! y)) allPairs0
    findMinimalCp :: [(Int, Int)] -> S.Set Int -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
    findMinimalCp cpSoFar _ [] = (cpSoFar, [])
    findMinimalCp cpSoFar nodesSoFar remain@((x, y) : rest) =
      let newNSF = S.insert x $ S.insert y nodesSoFar
      in if S.size newNSF == length dataPoints then (cpSoFar, remain)
            else findMinimalCp ((x, y) : cpSoFar) (S.insert x $ S.insert y nodesSoFar) rest
    (minCP, remainingConn) = findMinimalCp [] S.empty allPairs
    connectedGr0 = M.fromListWith S.union $ do
      (x, y) <- minCP
      [(x, S.singleton y), (y, S.singleton x)]
    gridNums0 = M.fromList [(x, x + 1) | x <- [0 .. length dataPoints - 1]]
    keepGoing gridNumCandidate = any (> 1) $ M.elems gridNumCandidate
    -- lastNeededPair :: M.Map Int (S.Set Int) -> [(Int, Int)] -> (Int, Int)
    lastNeededPair _ _ [] = error "Couldn't find answer"
    lastNeededPair gridNumsOld connectedGr ((x,y):rest) =
      let connectedGr' = M.unionWith S.union (M.fromList [(x, S.singleton y), (y, S.singleton x)]) connectedGr
          gridNums = findGridNums gridNumsOld connectedGr'
      in if keepGoing gridNums then lastNeededPair gridNums connectedGr' rest else (x,y)

-- gridNums = findGridNums gridNums0 connectedGr

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc8.in"
          (x : _) -> x
  dataLines <- lines <$> readFile filename
  let dataPoints = map (\x -> read $ "(" ++ x ++ ")") dataLines :: [(Int, Int, Int)]
  print $ part1 dataPoints
  print $ part2 dataPoints
