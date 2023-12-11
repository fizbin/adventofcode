import Data.List (tails, transpose)
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        let filename = if null args then "aoc11.in" else head args
        grid <- lines <$> readFile filename
        let galaxyLocs = [(rowidx, colidx) | (rowidx, row) <- zip [0 ..] grid, (colidx, ch) <- zip [0 ..] row, ch == '#']
            emptyRows = [idx | (idx, row) <- zip [0 ..] grid, all (== '.') row]
            emptyCols = [idx | (idx, col) <- zip [0 ..] (transpose grid), all (== '.') col]
            galaxyPairs = [(gal1, gal2) | (gal1 : rest) <- tails galaxyLocs, gal2 <- rest]
            gdist ((g1r, g1c), (g2r, g2c)) = abs (g1r - g2r) + abs (g1c - g2c) 
                                             + length [() | er <- emptyRows, (er - g1r) * (er - g2r) < 0]
                                             + length [() | ec <- emptyCols, (ec - g1c) * (ec - g2c) < 0]
        print $ sum $ map gdist galaxyPairs
        let gdist2 ((g1r, g1c), (g2r, g2c)) = abs (g1r - g2r) + abs (g1c - g2c) 
                                              + 999999 * length [() | er <- emptyRows, (er - g1r) * (er - g2r) < 0]
                                              + 999999 * length [() | ec <- emptyCols, (ec - g1c) * (ec - g2c) < 0]
        print $ sum $ map gdist2 galaxyPairs
