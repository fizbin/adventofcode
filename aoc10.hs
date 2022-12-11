import Control.Monad
import Data.Foldable (Foldable(foldl'))
import System.Environment (getArgs)

doline :: String -> [Int] -> [Int]
doline "noop" lst = head lst : lst
doline ('a':'d':'d':'x':s) lst = (read s + head lst) : head lst : lst
doline g _ = error $ "Bad cmd " ++ g

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc10.in"
                else head args
    s <- lines <$> readFile filename
    let xvals = reverse $ foldl' (flip doline) [1] s
    print $ sum $ map (\idx -> idx * (xvals !! (idx - 1))) [20, 60, 100, 140, 180, 220]
    putStrLn ""
    forM_ [0 .. 239] $ \idx -> do
        let col = idx `mod` 40
        if abs ((xvals !! idx) - col) <= 1
            then putChar '\9608'
            else putChar '.'
        when (col == 39) (putChar '\n')
