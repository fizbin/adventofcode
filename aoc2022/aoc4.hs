import Data.Char (isAsciiLower, isAsciiUpper, ord)
import Data.Set (findMin, fromList, intersection)
import System.Environment

parseLine :: String -> (Int, Int, Int, Int)
parseLine s0 = head $ parses1 s0
  where
    parses1 s = [(x, y, z, w) | (x, s') <- reads s, (y, z, w) <- parses2 s']
    parses2 s = [(y, z, w) | ('-':s') <- [s], (y, s'') <- reads s', (z, w) <- parses3 s'']
    parses3 s =
        [(z, w) | (',':s') <- [s], (z, s'') <- reads s', ('-':s''') <- [s''], (w, _) <- reads s''']

bad1 :: (Int, Int, Int, Int) -> Bool
bad1 (a, b, c, d) = ((a <= c) && (d <= b)) || ((c <= a) && (b <= d))

bad2 :: (Int, Int, Int, Int) -> Bool
bad2 (a, b, c, d) = not $ (b < c) || (d < a)

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc4.in"
                else head args
    s <- lines <$> readFile filename
    let parsed = map parseLine s
    --print $ map parseLine s
    print $ length $ filter bad1 parsed
    print $ length $ filter bad2 parsed
