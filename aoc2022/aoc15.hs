import Control.Monad
import Data.Char (isDigit)
import Data.List (foldl', nub)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

type Range = (Int, Int)

-- remove the first arg from the list of the other args
subRange :: [Range] -> Range -> [Range]
subRange [] _ = []
subRange ((c, d) : r) (a, b) | a > d || b < c = (c, d) : subRange r (a, b)
subRange ((c, d) : r) (a, b) | a <= c && b >= d = subRange r (a, b)
subRange ((c, d) : r) (a, b) | a <= c = (b + 1, d) : subRange r (a, b)
subRange ((c, d) : r) (a, b) | b >= d = (c, a - 1) : subRange r (a, b)
subRange ((c, d) : r) (a, b) | a > c && b < d = (c, a - 1) : (b + 1, d) : subRange r (a, b)
subRange ((c, d) : _) (a, b) = error $ "Error " ++ show ((a, b), (c, d))

numbers :: String -> [Int]
numbers [] = []
numbers s = case reads s of
    [] -> numbers $ dropWhile (\ch -> ch /= '-' && not (isDigit ch)) (tail s)
    (n, rest) : _ -> n : numbers rest

parseLine :: String -> (Range, Range)
parseLine str = case numbers str of
    [a, b, c, d] -> ((a, b), (c, d))
    _ -> error $ "Bad line: " ++ str

getExclusion :: Int -> (Range, Range) -> Maybe Range
getExclusion y (sensor, beacon) =
    let bdist = abs (fst sensor - fst beacon) + abs (snd sensor - snd beacon)
        rdist = bdist - abs (snd sensor - y)
     in if rdist < 0
            then Nothing
            else Just (fst sensor - rdist, fst sensor + rdist)

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc15.in"
                else head args
    s <- lines <$> readFile filename
    let cases = parseLine <$> s
    let maxnum = maximum $ concatMap numbers s
    let bigRange = (-3 * (maxnum + 1), 3 * maxnum + 1)
    let rangeSize (a, b) = b - a + 1
    let twomill = 2000000
    let p1Range = foldl' subRange [bigRange] (mapMaybe (getExclusion twomill) cases)
    let p1Beacons = [fst beacon | (_, beacon) <- cases, snd beacon == twomill]
    print $ rangeSize bigRange - (sum (map rangeSize p1Range) - length (nub p1Beacons))
    let fourmill = 4000000
    forM_ [0 .. fourmill] $ \y -> do
        let p2Range = foldl' subRange [(0, fourmill)] (mapMaybe (getExclusion y) cases)
        unless (null p2Range) $ do
            let xval = fst . head $ p2Range
            print $ fourmill * xval + y