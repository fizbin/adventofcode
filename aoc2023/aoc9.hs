import System.Environment (getArgs)

inferNext :: [Int] -> Int
inferNext xs | all (== 0) xs = 0
inferNext xs =
        let diffs = zipWith (-) (tail xs) xs
         in last xs + inferNext diffs

main :: IO ()
main = do
        args <- getArgs
        let filename = if null args then "aoc9.in" else head args
        s <- lines <$> readFile filename
        let datas = map (map read . words) s
        print $ sum $ map inferNext datas
        print $ sum $ map (inferNext . reverse) datas
