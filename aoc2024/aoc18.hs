import Data.Char (isDigit)
import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.PQueue.Min qualified as PM
import Data.Set qualified as S
import Dijkstra (dijkstra)
import System.Environment (getArgs)

findints :: String -> [Int]
findints "" = []
findints ('-' : x : xs) | isDigit x = case findints (x : xs) of
  [] -> error "Can't happen"
  (n : ns) -> (-n) : ns
findints (x : xs) | not (isDigit x) = findints xs
findints s = let (n, s') = head (reads s) in n : findints s'

fillMap :: [(Int, Int)] -> Map (Int, Int) Char
fillMap fallen = M.fromList ((,'#') <$> fallen) `M.union` M.fromList [((x, y), '.') | x <- [0 .. 70], y <- [0 .. 70]]

getNeighbors :: Map (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
getNeighbors mp (x, y) =
  let nbs = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
   in filter (\n -> Just '.' == M.lookup n mp) nbs

findThreshold :: [(Int, Int)] -> (Int, Int)
findThreshold lst = go 1 (length lst)
  where
    go a b | b - a <= 1 = lst !! a
    go a b =
      let m = (a + b) `div` 2
          one = 1 :: Int
          zero = 0 :: Int
          mymap = fillMap (take m lst)
       in case dijkstra (((,one) <$>) <$> getNeighbors mymap) (zero, zero) (== (70, 70)) of
            Nothing -> go a m
            _ -> go m b

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc18.in" else head args
  mydatastrs <- lines <$> readFile filename
  let parseLine ln = let xs = findints ln in (head xs, head (tail xs))
  let mydata = parseLine <$> mydatastrs
  let mymap = fillMap (take 1024 mydata)
  let zero = 0 :: Int
  let one = 1 :: Int
  putStr "Part 1: "
  print $ snd $ fromJust $ dijkstra (((,one) <$>) <$> getNeighbors mymap) (zero, zero) (== (70, 70))
  putStr "Part 2: "
  putStrLn $ filter ('(' /=) $ filter (')' /=) $ show $ findThreshold mydata
