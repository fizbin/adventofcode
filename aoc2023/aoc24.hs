import Data.Char (isDigit)
import Data.List (tails)
import Linear (
    Additive ((^-^)),
    Metric (dot),
    V2 (..),
    V3 (..),
    cross,
    det22,
    inv22,
    inv33,
    (!*),
 )
import System.Environment (getArgs)

findIntersection2D ::
    (V3 Rational, V3 Rational) ->
    (V3 Rational, V3 Rational) ->
    Maybe (V2 Rational, Rational, Rational)
findIntersection2D (V3 x1 y1 _, V3 vx1 vy1 _) (V3 x2 y2 _, V3 vx2 vy2 _) =
    -- x1 + t1 * vx1 = x2 + t2 * vx2
    -- y1 + t1 * vy1 = y2 + t2 * vy2
    --
    -- x1 - x2 = (-vx1  vx2)  (t1)
    -- y1 - y2 = (-vy1  vy2)  (t2)
    let vmat = V2 (V2 (-vx1) vx2) (V2 (-vy1) vy2)
        diffvec = V2 (x1 - x2) (y1 - y2)
        tvec = inv22 vmat !* diffvec
        V2 t1 t2 = tvec :: V2 Rational
        xInter = x1 + t1 * vx1
        yInter = y1 + t1 * vy1
     in if det22 vmat == 0 then Nothing else Just (V2 xInter yInter, t1, t2)

readLine :: String -> (V3 Rational, V3 Rational)
readLine str =
    let nonnum c = not ((c == '-') || isDigit c)
        takebit (_, s) = head (reads $ dropWhile nonnum s) :: (Int, [Char])
        numbers = map fst $ iterate takebit (0, str)
     in ( fmap fromIntegral (V3 (numbers !! 1) (numbers !! 2) (numbers !! 3))
        , fmap fromIntegral (V3 (numbers !! 4) (numbers !! 5) (numbers !! 6))
        )

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc24.in" else head args
    s <- lines <$> readFile filename
    let stones = map readLine s
    let minCoord = 200000000000000
        maxCoord = 400000000000000
        acceptable a b = case findIntersection2D a b of
            Just (V2 x y, t1, t2) ->
                x >= minCoord
                    && y >= minCoord
                    && x <= maxCoord
                    && y <= maxCoord
                    && t1 >= 0
                    && t2 >= 0
            Nothing -> False
    print $ length [() | (a : as) <- tails stones, b <- as, acceptable a b]
    -- r + t1*w = s1 + t1*v1  <=>  r = s1 + t1*(v1-w)
    -- r + t2*w = s2 + t2*v2  <=>  r = s2 + t2*(v2-w)
    -- (s1 - s2), (v1 - w), and (v2 - w) all linearly dependent
    -- <=> (s1 - s2), (v1 - v2), and (v1 - w) all linearly dependent
    -- ((s1-s2) x (v1-v2)) . (v1-w) == 0
    -- ((s1-s2) x (v1-v2)) . v1 == ((s1-s2) x (v1-v2)) . w
    let (s1, v1) = head stones
        (s2, v2) = stones !! 1
        (s3, v3) = stones !! 2
        cross1 = (s1 ^-^ s2) `cross` (v1 ^-^ v2)
        cross2 = (s2 ^-^ s3) `cross` (v2 ^-^ v3)
        cross3 = (s3 ^-^ s1) `cross` (v3 ^-^ v1)
        crossVec = V3 (cross1 `dot` v1) (cross2 `dot` v2) (cross3 `dot` v3)
        crossMat = V3 cross1 cross2 cross3
        w = inv33 crossMat !* crossVec
        finval = findIntersection2D (s1, v1 ^-^ w) (s2, v2 ^-^ w)
        V3 _ _ z1 = s1
        V3 _ _ vz1 = v1
        V3 _ _ wz = w
    case finval of
        Nothing -> print "~No ans found~"
        Just (V2 xI yI, t1, _) ->
            let zI = z1 + t1 * (vz1 - wz)
             in print (round (xI + yI + zI) :: Int)
