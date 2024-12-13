import Data.Char
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

solve :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe Int
solve ax ay bx by px py =
  let detcoeff = ax * by - ay * bx
      detwob = ax * py - ay * px
      detwoa = by * px - bx * py
      apresses = detwoa `div` detcoeff
      bpresses = detwob `div` detcoeff
   in if (apresses >= 0)
        && (bpresses >= 0)
        && (apresses * ax + bpresses * bx == px)
        && (apresses * ay + bpresses * by == py)
        then Just (3 * apresses + bpresses)
        else Nothing

solve2 :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe Int
solve2 ax ay bx by px py = let newadd = 10000000000000 in solve ax ay bx by (newadd + px) (newadd + py)

findints :: String -> [Int]
findints "" = []
findints (x : xs) | not (isDigit x) = findints xs
findints s = let (n, s') = head (reads s) in n : findints s'

certifyInt6 :: [Int] -> IO (Int, Int, Int, Int, Int, Int)
certifyInt6 lst = do
  if length lst /= 6
    then ioError (userError "Bad parse")
    else pure (head lst, lst !! 1, lst !! 2, lst !! 3, lst !! 4, lst !! 5)

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc13.in" else head args
  paras <- splitOn "\n\n" <$> readFile filename
  let problems' = map findints paras
  problems <- mapM certifyInt6 problems'
  let total1 = sum $ mapMaybe (\(n1, n2, n3, n4, n5, n6) -> solve n1 n2 n3 n4 n5 n6) problems
  print total1
  let total2 = sum $ mapMaybe (\(n1, n2, n3, n4, n5, n6) -> solve2 n1 n2 n3 n4 n5 n6) problems
  print total2
