import qualified Data.Set as S
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc6.in" else head args
    s <- readFile filename
    print $ head $ filter (\idx -> S.size (S.fromList (drop (idx - 4) $ take idx s)) == 4) [4..]
    print $ head $ filter (\idx -> S.size (S.fromList (drop (idx - 14) $ take idx s)) == 14) [14..]
