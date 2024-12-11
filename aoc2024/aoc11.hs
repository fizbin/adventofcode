{-# LANGUAGE BangPatterns #-}
import System.Environment (getArgs)
import qualified Data.Map.Strict as M

advanceNum :: Int -> [Int]
advanceNum 0 = [1]
advanceNum s =
  let sstr = show s
      (halflen, mod2) = length sstr `divMod` 2
   in if mod2 == 0
        then [read (take halflen sstr), read (drop halflen sstr)]
        else [s * 2024]

runNumCount :: Int -> [Int] -> Integer
runNumCount _ [] = 0
runNumCount count (x:xs) = doIt + runNumCount count xs
  where
    doIt = let finalMap = iterate mapAdvance (M.singleton x 1) !! count
            in sum $ snd <$> M.toList finalMap
    mapAdvance :: M.Map Int Integer -> M.Map Int Integer
    mapAdvance !repMap = M.fromListWith (+) $ concatMap (\(k, v) -> (,v) <$> advanceNum k) (M.toList repMap)

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc11.in" else head args
  let reader s = [(a : as, s') | (a, s'') <- reads s, (as, s') <- reader s''] ++ [([], s)]
  (nums, _) <- head . reader <$> readFile filename :: IO ([Int], String)
  putStrLn $ "Part 1: " ++ show (runNumCount 25 nums)
  putStrLn $ "Part 2: " ++ show (runNumCount 75 nums)
