import Control.Monad (guard)
import Data.List (isSuffixOf)
import System.Environment (getArgs)

parseLine :: String -> (Int, [Int])
parseLine s = head parsed
  where
    parsed = [(n, rdlst ns) | (n, ':' : ns) <- reads s]
    rdlst (' ' : ss) = rdlst ss
    rdlst [] = []
    rdlst x = head [n : rdlst ns | (n, ns) <- reads x]

checkAns1 :: Int -> [Int] -> Bool
checkAns1 _ [] = False
checkAns1 goal nums = doCheck [goal] (reverse nums)
  where
    doCheck [] _ = False
    doCheck _ [] = error "WTF?"
    doCheck tgts [x] = x `elem` tgts
    doCheck tgts (x : xs) =
      let tgts1 = flip (-) x <$> tgts
          tgts2 = do
            (d, 0) <- (`divMod` x) <$> tgts
            pure d
       in doCheck (tgts1 ++ tgts2) xs

checkAns2 :: Int -> [Int] -> Bool
checkAns2 _ [] = False
checkAns2 goal nums = doCheck [goal] (reverse nums)
  where
    doCheck [] _ = False
    doCheck _ [] = error "WTF?"
    doCheck tgts [x] = x `elem` tgts
    doCheck tgts (x : xs) =
      let tgts1 = (flip (-) x <$> tgts)
          tgts2 = do
            (d, 0) <- (`divMod` x) <$> tgts
            pure d
          tgts3 = do
            let xstr = show x
            t <- tgts
            guard $ t /= x
            let tstr = show t
            guard $ xstr `isSuffixOf` tstr
            pure $ read $ take (length tstr - length xstr) tstr
       in doCheck (tgts1 ++ tgts2 ++ tgts3) xs

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc7.in" else head args
  grid <- lines <$> readFile filename
  let rows = parseLine <$> grid
  putStrLn $ "Part 1: " ++ show (sum $ fst <$> filter (uncurry checkAns1) rows)
  putStrLn $ "Part 2: " ++ show (sum $ fst <$> filter (uncurry checkAns2) rows)
