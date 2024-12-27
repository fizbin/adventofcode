import Control.Monad (guard)
import Data.List
import Data.Map.Strict qualified as M
-- import Debug.Trace
import System.Environment (getArgs)

fullyConnected :: M.Map String [String] -> [String] -> Bool
fullyConnected _ [] = True
fullyConnected network (x : xs) =
  let xc = M.findWithDefault [] x network
   in fullyConnected network xs && all (`elem` xc) xs

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (a : as) = combinations n as ++ ((a :) <$> combinations (n - 1) as)

part2 :: M.Map String [String] -> [String]
part2 network = go [] (M.toAscList network)
  where
    comm [] _ = []
    comm _ [] = []
    comm af@(a : as) bf@(b : bs)
      | a < b = comm as bf
      | a > b = comm af bs
      | otherwise = a : comm as bs
    go :: [String] -> [(String, [String])] -> [String]
    go acc [] = acc
    go acc ((_, []) : rs) = go acc rs
    go acc ((nd, ns) : net') =
      let minSz = length acc + 1
          repTests = do
            -- traceM $ "Top Of Node " ++ show nd ++ " minsz " ++ show minSz
            sz <- [length ns, length ns - 1 .. minSz]
            -- traceM $ "sz is " ++ show sz
            nd2 <- ns
            let c1 = filter (\x -> x > nd && x > nd2) $ comm ns (M.findWithDefault [] nd2 network)
            c2 <- combinations (sz - 2) c1
            let c3 = nd : nd2 : c2
            -- traceM $ "Trying " ++ show c3
            guard $ fullyConnected network c3
            pure c3
       in case repTests of
            [] -> go acc net'
            (acc' : _) -> go acc' net'

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc23.in" else head args
  mydata <- lines <$> readFile filename
  let network = M.map sort $ M.fromListWith (++) $ do
        thing <- mydata
        (von, _ : zu) <- [break (== '-') thing]
        [(von, [zu]), (zu, [von])]
  let tnodeTriples = do
        (tnode@('t' : _), tgts') <- M.toList network
        let tgts = filter (\x -> head x /= 't' || x > tnode) tgts'
        (a : bs) <- tails tgts
        b <- bs
        guard $ b `elem` M.findWithDefault [] a network
        pure (tnode, a, b)
  putStr "Part 1: "
  print $ length tnodeTriples
  putStr "Part 2: "
  putStrLn $ filter (`notElem` "[]\"") . show $ part2 network