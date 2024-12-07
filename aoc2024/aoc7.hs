import System.Environment ( getArgs )

parseLine :: String -> (Int, [Int])
parseLine s = head parsed
  where
    parsed = [(n, rdlst ns) | (n, ':':ns) <- reads s]
    rdlst (' ':ss) = rdlst ss
    rdlst [] = []
    rdlst x = head [n:rdlst ns | (n, ns) <- reads x]

checkAns1 :: Int -> [Int] -> Bool
checkAns1 _ [] = False
checkAns1 goal (n:nums) = doCheck nums n
  where
    doCheck [] a = a == goal
    doCheck _ sofar | sofar > goal = False
    doCheck (x:xs) sofar = doCheck xs (sofar + x) || doCheck xs (sofar*x)

concatNum :: Int -> Int -> Int
concatNum a b = read $ show a ++ show b

checkAns2 :: Int -> [Int] -> Bool
checkAns2 _ [] = False
checkAns2 goal (n:nums) = doCheck nums n
  where
    doCheck [] a = a == goal
    doCheck _ sofar | sofar > goal = False
    doCheck (x:xs) sofar = doCheck xs (sofar + x) || doCheck xs (sofar*x) || doCheck xs (concatNum sofar x) 

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc7.in" else head args
    grid <- lines <$> readFile filename
    let rows =  parseLine <$> grid
    putStrLn $ "Part 1: " ++ show (sum $ fst <$> filter (uncurry checkAns1) rows)
    putStrLn $ "Part 2: " ++ show (sum $ fst <$> filter (uncurry checkAns2) rows)
