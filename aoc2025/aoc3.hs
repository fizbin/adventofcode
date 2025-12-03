import Data.List (tails)
import System.Environment (getArgs)

part1 :: String -> Int
part1 [] = 0
part1 line = maximum $ map go (tails line)
  where
    go :: String -> Int
    go [] = -1
    go [_] = -1
    go (x:xs) = read [x, maximum xs]

part2 :: String -> Integer
part2 [] = 0
part2 line0 = read $ go 12 line0
  where
    go :: Int -> String -> String
    go 0 _ = "shouldn't get here"
    go _ "" = error "shouldn't get here either"
    go 1 line = [maximum line]
    go i line =
      let maxChar = maximum $ take (length line - i + 1) line
      in case dropWhile (/= maxChar) line of
        (firstChar:rest) -> firstChar : go (i-1) rest
        [] -> error "also shouldn't get here"

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc3.in"
          (x:_) -> x
  batteries <- lines <$> readFile filename
  print $ sum $ map part1 batteries
  print $ sum $ map part2 batteries
