import Data.Char (isSpace)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
-- import Debug.Trace
import Control.Category ((>>>))
import Control.Monad ((>=>))

read3Int :: String -> (Int, Int, Int)
read3Int s =
    head
        [ (a, b, c) | (a, sa) <- reads s, (b, sb) <- reads sa, (c, sc) <- reads sb, all isSpace sc
        ]

-- Output is (seeds unmapped, seeds mapped)
mkFuncP1 :: (Int, Int, Int) -> Int -> ([Int], [Int])
mkFuncP1 (dstart, sstart, rlen) seed =
    if sstart <= seed && seed < sstart + rlen
        then ([], [seed + dstart - sstart])
        else ([seed], [])

-- Join two functions with the convention "input -> (input to retry, output)"
(>+>) :: (a -> ([a], [b])) -> (a -> ([a], [b])) -> (a -> ([a], [b]))
(f1 >+> f2) val =
    let (firstL, firstR) = f1 val
        secondResult = map f2 firstL
     in foldr (\(a, b) (c, d) -> (a ++ c, b ++ d)) ([], firstR) secondResult

mkMapP1 :: String -> Int -> Int
mkMapP1 mapStr =
    let mdata = tail $ lines mapStr
        mfuncs = map (mkFuncP1 . read3Int) mdata
        allMFuncs = foldl (>+>) (\x -> ([x], [])) mfuncs
     in (\x -> let (lefts, rights) = allMFuncs x in head (lefts ++ rights))

-- output is (range-len pairs unmapped, range-len pairs mapped)
mkFuncP2 :: (Int, Int, Int) -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
mkFuncP2 (dstart, sstart, rlen) = f
  where
    f my@(myStart, myLen) | myStart + myLen <= sstart = ([my], [])
    f my@(myStart, _) | sstart + rlen <= myStart = ([my], [])
    f (myStart, myLen)
        | myStart < sstart =
            let excess = sstart - myStart
                (othRetry, othDone) = f (sstart, myLen - excess)
             in ((myStart, excess) : othRetry, othDone)
    f (myStart, myLen)
        | myStart + myLen > sstart + rlen =
            let excess = myStart + myLen - (sstart + rlen)
                (othRetry, othDone) = f (myStart, myLen - excess)
             in ((sstart + rlen, excess) : othRetry, othDone)
    f (myStart, myLen) = ([], [(myStart + dstart - sstart, myLen)])

mkMapP2 :: String -> (Int, Int) -> [(Int, Int)]
mkMapP2 mapStr =
    let mdata = tail $ lines mapStr
        mfuncs = map (mkFuncP2 . read3Int) mdata
        allMFuncs = foldl (>+>) (\x -> ([x], [])) mfuncs
     in (\x -> let (lefts, rights) = allMFuncs x in (lefts ++ rights))

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc5.in" else head args
    (seedData : mapData) <- splitOn "\n\n" <$> readFile filename
    let seedNums = map read $ words $ tail $ dropWhile (/= ':') seedData
    -- let finalSeeds = map (\x -> foldl' (flip ($)) x (map mkMapP1 mapData)) seedNums
    let finalSeeds = foldr1 (>>>) (map mkMapP1 mapData) <$> seedNums
    print $ minimum finalSeeds
    let mkPairs (a : b : r) = (a, b) : mkPairs r; mkPairs [] = []; mkPairs _ = error "odd length"
    let seedPairs = mkPairs seedNums
    -- let finalPairs = (\x -> foldM (flip ($)) x (map mkMapP2 mapData)) =<< seedPairs
    let finalPairs = foldr1 (>=>) (map mkMapP2 mapData) =<< seedPairs
    print $ minimum $ map fst finalPairs
