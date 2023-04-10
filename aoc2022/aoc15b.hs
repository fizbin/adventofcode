import Control.Monad
import Data.Bifunctor
import Data.Char (isDigit)
import Data.List (foldl', nub, sort)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Debug.Trace (trace)

type Pt2 = (Int, Int)
type Range = (Int, Int)
data Rectangle = Rect
    { umin :: Int
    , umax :: Int
    , vmin :: Int
    , vmax :: Int
    } deriving (Show, Eq, Ord)

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

parseLine :: String -> (Pt2, Pt2)
parseLine str = case numbers str of
    [a, b, c, d] -> ((a, b), (c, d))
    _ -> error $ "Bad line: " ++ str

getExclusion :: Int -> (Pt2, Pt2) -> Maybe Range
getExclusion y (sensor, beacon) =
    let bdist = abs (fst sensor - fst beacon) + abs (snd sensor - snd beacon)
        rdist = bdist - abs (snd sensor - y)
     in if rdist < 0
            then Nothing
            else Just (fst sensor - rdist, fst sensor + rdist)

manDist :: Pt2 -> Pt2 -> Int
manDist s b = abs (fst s - fst b) + abs (snd s - snd b)

subRect :: [Rectangle] -> Rectangle -> [Rectangle]
subRect [] _ = []
subRect (a : r) b | umin a > umax b || umin b > umax a || vmin a > vmax b || vmin b > vmax a = a : subRect r b
subRect (a : r) b | umin b <= umin a && umax a <= umax b && vmin b <= vmin a && vmax a <= vmax b = subRect r b
subRect (a : r) b
    | umin a < umin b && umin b <= umax a =
        -- so the line u=umin b intersects a
        let l1 = Rect (umin a) (umin b - 1) (vmin a) (vmax a)
            l2 = Rect (umin b) (umax a) (vmin a) (vmax a)
         in subRect (l1 : l2 : r) b
subRect (a : r) b
    | vmin a < vmin b && vmin b <= vmax a =
        -- so the line v=vmin b intersects a
        let l1 = Rect (umin a) (umax a) (vmin a) (vmin b - 1)
            l2 = Rect (umin a) (umax a) (vmin b) (vmax a)
         in subRect (l1 : l2 : r) b
subRect (a : r) b
    | umin a <= umax b && umax b < umax a =
        -- so the line u=umax b intersects a
        let l1 = Rect (umin a) (umax b) (vmin a) (vmax a)
            l2 = Rect (umax b + 1) (umax a) (vmin a) (vmax a)
         in subRect (l1 : l2 : r) b
subRect (a : r) b
    | vmin a <= vmax b && vmax b < vmax a =
        -- so the line v=vmax b intersects a
        let l1 = Rect (umin a) (umax a) (vmin a) (vmax b)
            l2 = Rect (umin a) (umax a) (vmax b + 1) (vmax a)
         in subRect (l1 : l2 : r) b
subRect (a : r) b = error $ "Logic error on intersection with " ++ show a ++ " and " ++ show b

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

    let makeSensorSquare sensor beacon =
            let bdist = manDist sensor beacon
             in Rect (uncurry (+) sensor - bdist) (uncurry (+) sensor + bdist) (uncurry (-) sensor - bdist) (uncurry (-) sensor + bdist)
    let sensorSquares = map (uncurry makeSensorSquare) cases
    let fourmill = 4000000
    let finalRects = foldl' subRect [Rect 0 (2*fourmill) (-fourmill) fourmill] sensorSquares
    let inRangeRects = filter (not . (\r -> ((umax r + vmax r) < 0) || ((umin r + vmin r) > 2*fourmill) || (umax r - vmin r < 0) || (umin r - vmax r > 2*fourmill) )) finalRects
    unless (length inRangeRects == 1) (ioError $ userError "Bad number of rectangles left")
    let theRect = head inRangeRects
    unless (umin theRect == umax theRect && vmin theRect == vmax theRect) (ioError $ userError $ "Rectangle too big " ++ show theRect)
    let xval = (umin theRect + vmin theRect) `div` 2
    let yval = (umin theRect - vmin theRect) `div` 2
    print $ fourmill*xval + yval