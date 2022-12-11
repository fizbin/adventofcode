import Control.Monad
import Data.List
import System.Environment (getArgs)

rotccw :: [[a]] -> [[a]]
rotccw = reverse . transpose

rotcw :: [[a]] -> [[a]]
rotcw = transpose . reverse

apply4 evalf combf lst =
    let eval1 f = map (\row -> map f (init $ tails row))
        a = eval1 evalf lst
        b = rotcw $ eval1 evalf $ rotccw lst
        c = rotcw . rotcw $ eval1 evalf $ rotccw $ rotccw lst
        d = rotccw $ eval1 evalf $ rotcw lst
     in zipWith4 (zipWith4 (\p q r s -> foldr1 combf [p, q, r, s])) a b c d

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc8.in"
                else head args
    s <- lines <$> readFile filename
    let visgrid = apply4 (\(x:xx) -> x > maximum (' ' : xx)) (||) s
    print $ length $ filter id $ join visgrid
    let scoregrid = apply4 (\(x:xx) -> min (length xx) ((1 +) $ length $ takeWhile (< x) xx)) (*) s
    --print scoregrid
    print $ maximum $ maximum <$> scoregrid
