import Control.Monad (guard)
import Data.List (tails)
import Data.Map.Lazy hiding (take)
import System.Environment (getArgs)

part1 :: String -> Int
part1 [] = 0
part1 line = maximum $ fmap go (tails line)
  where
    rd :: Char -> Int
    rd = read . (: [])
    go :: String -> Int
    go [] = -1
    go [_] = -1
    go (x:xs) = read [x] * 10 + maximum (fmap (fromIntegral . rd) xs)

part2 :: String -> Integer
part2 [] = 0
part2 line0 = ansMap ! (12, line0)
  where
    pow10 :: Int -> Integer
    pow10 x = 10 ^ x
    go :: (Int, String) -> Integer
    go (0, _) = -1
    go (_, "") = -1
    go (1, line) = maximum $ fmap (read . (: [])) line
    go (i, line) =
      maximum $ do
        let maxStart = go (1, take (length line - i + 1) line)
        let powBoost = pow10 (i - 1)
        (firstChar:rest) <- take (length line - i + 1) $ tails line
        let firstVal = read [firstChar]
        guard (firstVal == maxStart)
        return $ powBoost * firstVal + (ansMap ! (i - 1, rest))
    ansMap :: Map (Int, String) Integer
    ansMap =
      fromList $ do
        ival <- [0 .. 12]
        line <- tails line0
        return ((ival, line), go (ival, line))

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc3.in"
          (x:_) -> x
  batteries <- lines <$> readFile filename
  print $ sum $ fmap part1 batteries
  print $ sum $ fmap part2 batteries
