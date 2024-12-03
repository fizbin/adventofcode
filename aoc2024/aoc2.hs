import System.Environment ( getArgs )
import Data.Char ( isSpace )
import Data.List ( inits, tails )

readsLine :: String -> [([Int], String)]
readsLine "" = [([], "")]
readsLine (c:s) | isSpace c = readsLine s
readsLine s = [(x:y, s'') | (x, s') <- reads s, (y, s'') <- readsLine s']

isSafeUp :: [Int] -> Bool
isSafeDown :: [Int] -> Bool
isSafe :: [Int] -> Bool

isSafeUp [] = True
isSafeUp [_] = True
isSafeUp (x:rs@(y:_)) = (y - x >= 1) && (y - x <= 3) && isSafeUp rs

isSafeDown [] = True
isSafeDown [_] = True
isSafeDown (x:rs@(y:_)) = (x - y >= 1) && (x - y <= 3) && isSafeDown rs

isSafe x = isSafeUp x || isSafeDown x

isSafe2 :: [Int] -> Bool
isSafe2 [] = True
isSafe2 a = isSafe a || any isSafe (zipWith (++) (inits a) (tail $ tails a))

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc2.in" else head args
    grid <- lines <$> readFile filename
    let rows'' = head . readsLine <$> grid
    let rows = fst <$> rows''
    putStrLn $ "Part 1: " ++ show (length $ filter isSafe rows)
    putStrLn $ "Part 2: " ++ show (length $ filter isSafe2 rows)
