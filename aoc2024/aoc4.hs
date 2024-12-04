import Control.Monad (guard)
import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.List (tails, transpose)
import System.Environment (getArgs)

horizwords :: [[Char]] -> [String]
horizwords = concatMap $ map (take 4) . tails

vertwords :: [[Char]] -> [String]
vertwords = horizwords . transpose

diag1words :: [[Char]] -> [String]
diag1words s = concatMap eachtop (tails s)
  where
    eachtop :: [String] -> [String]
    eachtop st = map (take 4) $ transpose $ zipWith ($) (iterate (tail .) id) st

diag2words :: [[Char]] -> [String]
diag2words = diag1words . reverse

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc4.in" else head args
  grid <- lines <$> readFile filename
  putStr "Part 1: "
  print $
    length $
      filter (\s -> s == "XMAS" || s == "SAMX") $
        horizwords grid ++ vertwords grid ++ diag1words grid ++ diag2words grid
  let gridWithBuffer = map (\x -> ' ' : x ++ " ") $ (\y -> y : grid ++ [y]) (replicate (length $ head grid) ' ')
  let gridArray :: UArray (Int, Int) Char
      gridArray = listArray ((-1, -1), (length grid, length $ head grid)) (concat gridWithBuffer)
  let foundList = do
        ((x, y), v) <- assocs gridArray
        guard $ v == 'A'
        guard $ gridArray ! (x - 1, y - 1) `elem` "MS"
        guard $ gridArray ! (x - 1, y + 1) `elem` "MS"
        guard $ gridArray ! (x + 1, y - 1) `elem` "MS"
        guard $ gridArray ! (x + 1, y + 1) `elem` "MS"
        guard $ gridArray ! (x + 1, y - 1) /= gridArray ! (x - 1, y + 1)
        guard $ gridArray ! (x - 1, y - 1) /= gridArray ! (x + 1, y + 1)
        pure ()
  putStr "Part 2: "
  print $ length foundList