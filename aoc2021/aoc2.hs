import System.Environment
import Data.List

p1 :: [(String, Int)] -> Integer
p1 thing = let (h, d) = foldl' doit (0, 0) thing
            in h*d
  where
    doit (h1, d1) ("forward", n) = (h1 + fromIntegral n, d1)
    doit (h1, d1) ("up", n) = (h1, d1 - fromIntegral n)
    doit (h1, d1) ("down", n) = (h1, d1 + fromIntegral n)
    doit _ x = error $ "Unknown " ++ show x

p2 :: [(String, Int)] -> Integer
p2 thing = let (h, d, _) = foldl' doit (0, 0, 0) thing
            in h*d
    where
    doit (h1, d1, a1) ("forward", n) = (h1 + fromIntegral n, d1 + a1 * fromIntegral n, a1)
    doit (h1, d1, a1) ("up", n) = (h1, d1, a1 - fromIntegral n)
    doit (h1, d1, a1) ("down", n) = (h1, d1, a1 + fromIntegral n)
    doit _ x = error $ "Unknown " ++ show x
    
main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc2.in" else head args
  s <- readFile filename
  let mylines = lines s
  let instrs = map (\ln -> let wrds = words ln in (head wrds, read (head $ tail wrds))) mylines
  print $ p1 instrs
  print $ p2 instrs
