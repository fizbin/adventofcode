import Data.List
import System.Environment (getArgs)

visibleFromLeft :: Ord a => [a] -> [Bool]
visibleFromLeft [] = []
visibleFromLeft (s:ss) = True : go s ss
  where
    go _ [] = []
    go m (x:xx) = (x > m) : go (max m x) xx

visible2d :: Ord a => [[a]] -> [[Bool]]
visible2d lst = or2d (lrcheck lst) (transpose (lrcheck (transpose lst)))
  where
    lrcheck :: Ord a => [[a]] -> [[Bool]]
    lrcheck = map lrcheck1
    or2d :: [[Bool]] -> [[Bool]] -> [[Bool]]
    or2d = zipWith (zipWith (||))
    lrcheck1 :: Ord a => [a] -> [Bool]
    lrcheck1 lst = zipWith (||) (visibleFromLeft lst) (reverse $ visibleFromLeft $ reverse lst)

leftVisScore :: Ord a => [a] -> [Int]
leftVisScore [] = []
leftVisScore (s:ss) = 0 : go [s] ss
  where
    go _ [] = []
    -- visible to left is either number of trees or 1 + length (takeWhile (< x) prev)
    go prev (x:xx) = min (length prev) (length (x : takeWhile (< x) prev)) : go (x : prev) xx

score2d :: Ord a => [[a]] -> [[Int]]
score2d lst = mul2d (lrcheck lst) (transpose (lrcheck (transpose lst)))
  where
    lrcheck :: Ord a => [[a]] -> [[Int]]
    lrcheck = map lrcheck1
    mul2d :: [[Int]] -> [[Int]] -> [[Int]]
    mul2d = zipWith (zipWith (*))
    lrcheck1 :: Ord a => [a] -> [Int]
    lrcheck1 lst = zipWith (*) (leftVisScore lst) (reverse $ leftVisScore $ reverse lst)

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc8.in"
          else head args
  s <- lines <$> readFile filename
  print $ sum $ map (length . filter id) $ visible2d s
  print $ maximum $ map maximum $ score2d s
