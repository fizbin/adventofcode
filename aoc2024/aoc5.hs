import Control.Arrow (second)
import Control.Monad (guard)
import Data.List (partition, tails)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import System.Environment (getArgs)

midList :: [a] -> a
midList x = x !! (length x `div` 2)

isSorted :: Map Int (Set Int) -> [Int] -> Bool
isSorted rules book =
  let counter = do
        (a : ts) <- tails book
        b <- ts
        guard $ a `S.member` M.findWithDefault S.empty b rules
        pure (a, b)
   in null counter

sortBook :: Map Int (Set Int) -> [Int] -> [Int]
sortBook _ [] = []
sortBook rules (a : as) = case break (\p -> a `S.member` M.findWithDefault S.empty p rules) as of
  (postA, []) -> a : sortBook rules postA
  (postA, b:unk) -> sortBook rules (b:a:unk ++ postA)

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc5.in" else head args
  grid <- lines <$> readFile filename
  let (rules, books) = second (\x -> if null x then [] else tail x) $ break null grid
  let ruleMap = M.map S.fromList $ M.fromListWith (++) [(read a, [read b]) | rule <- rules, (a, _ : b) <- [break (== '|') rule]]
  let (goodBooks, badBooks) = partition (isSorted ruleMap) $ map (read . (\x -> '[' : x ++ "]")) books
  putStrLn $ "Part 1: " ++ show (sum $ map midList goodBooks)
  putStrLn $ "Part 2: " ++ show (sum $ map (midList . sortBook ruleMap) badBooks)
