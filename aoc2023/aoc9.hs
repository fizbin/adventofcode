import System.Environment (getArgs)

inferNext :: [Int] -> Int
inferNext [] = 0
inferNext xs = last xs + inferNext (zipWith (-) (tail xs) xs)

main :: IO ()
main = do
        args <- getArgs
        let filename = if null args then "aoc9.in" else head args
        s <- lines <$> readFile filename
        let datas = map (map read . words) s
        print $ sum $ map inferNext datas
        print $ sum $ map (inferNext . reverse) datas
