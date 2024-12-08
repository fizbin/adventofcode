import Control.Monad (guard)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List (tails)
import System.Environment (getArgs)

repeatWhile :: (a -> a) -> (a -> Bool) -> a -> [a]
repeatWhile _ condition start | not (condition start) = []
repeatWhile mkNext condition start = start : repeatWhile mkNext condition (mkNext start)

both2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
both2 fn (x1, x2) (y1, y2) = (fn x1 y1, fn x2 y2)

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc8.in" else head args
  grid <- lines <$> readFile filename
  let antennas = HM.fromListWith (++) $ do
        (idx, row) <- zip [(0 :: Int) ..] grid
        (cidx, spot) <- zip [(0 :: Int) ..] row
        guard (spot /= '.')
        pure (spot, [(idx, cidx)])
  let height = length grid
  let width = length $ head grid
  let isInside (x, y) = (0 <= x) && (0 <= y) && (x < height) && (y < width)
  let antinodes1 = HS.toList $ HS.fromList $ filter isInside $ do
        (_, antennas') <- HM.toList antennas
        (spot1 : rest) <- tails antennas'
        spot2 <- rest
        let twoAMinB a b = 2 * a - b
        [both2 twoAMinB spot1 spot2, both2 twoAMinB spot2 spot1]
  putStrLn $ "Part 1: " ++ show (length antinodes1)
  let antinodes2 = HS.toList $ HS.fromList $ filter isInside $ do
        (_, antennas') <- HM.toList antennas
        (spot1 : rest) <- tails antennas'
        spot2 <- rest
        let nxt1 = both2 (+) $ both2 (-) spot1 spot2
        let nxt2 = both2 (+) $ both2 (-) spot2 spot1
        let left = repeatWhile nxt1 isInside spot1
        let right = repeatWhile nxt2 isInside spot2
        left ++ right
  putStrLn $ "Part 2: " ++ show (length antinodes2)