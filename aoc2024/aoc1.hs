import System.Environment (getArgs)
import Control.Arrow (second)
import Data.List (sort)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc1.in" else head args
    grid <- lines <$> readFile filename
    let nargs = map (second read . head . reads) grid
    let r1 = sort $ fst <$> nargs :: [Int]
    let r2 = sort $ snd <$> nargs
    putStrLn $ "Part 1: " ++ show (sum $ abs <$> zipWith (-) r1 r2)
    putStrLn $ "Part 2: " ++ show (sum [x | x <- r1, y <- r2, x == y])