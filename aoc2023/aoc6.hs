import System.Environment (getArgs)
import Data.Char (isDigit)

findHoldMin :: Int -> Int -> Int
findHoldMin time dist = snd (res (0, time `div` 2 - 1))
    where
        res (lo, hi) | hi - lo <= 1 = (lo, hi)
        res (lo, hi) =
                let mid = (lo + hi) `div` 2
                 in if mid * (time - mid) > dist then res (lo, mid) else res (mid, hi)

findHoldMax :: Int -> Int -> Int
findHoldMax time dist = fst (res (time `div` 2 - 1, time))
    where
        res (lo, hi) | hi - lo <= 1 = (lo, hi)
        res (lo, hi) =
                let mid = (lo + hi) `div` 2
                 in if mid * (time - mid) > dist then res (mid, hi) else res (lo, mid)

findHoldPoss :: Int -> Int -> Int
findHoldPoss time dist = 1 + findHoldMax time dist - findHoldMin time dist

main :: IO ()
main = do
        args <- getArgs
        let filename = if null args then "aoc6.in" else head args
        s <- lines <$> readFile filename
        let times = read <$> tail (words $ head s)
        let dists = read <$> tail (words $ head (tail s))
        let p1holdtimes = uncurry findHoldPoss <$> zip times dists
        print $ product p1holdtimes
        let bigtime = read $ filter isDigit (head s)
        let bigdist = read $ filter isDigit (head $ tail s)
        print $ findHoldPoss bigtime bigdist
