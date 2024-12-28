import Data.Either (lefts, rights)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

newtype Lock = Lock [Int] deriving (Show)

newtype Key = Key [Int] deriving (Show)

countPounds :: [String] -> [Int]
countPounds =
  foldr
    (\v -> zipWith (+) [if ch == '#' then 1 else 0 | ch <- v])
    (repeat 0)

parsePara :: [String] -> Either Lock Key
parsePara [] = error "empty para"
parsePara para | all (== '#') (head para) = Left $ Lock $ countPounds (tail para)
parsePara para | all (== '#') (last para) = Right $ Key $ countPounds (init para)
parsePara para = error $ "Bad para: " ++ show para

fits :: Lock -> Key -> Bool
fits (Lock ls) (Key ks) = maximum (zipWith (+) ls ks) < 6

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc25.in" else head args
  pieces <- splitOn "\n\n" <$> readFile filename
  let parsed = parsePara . lines <$> pieces
  putStr "Part 1: "
  print $ length [() | l <- lefts parsed, k <- rights parsed, fits l k]
