import Data.Array (Ix (rangeSize), accum, listArray, (!))
import Data.Char (isDigit)
import Data.List (nub, sort, sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S
import System.Environment (getArgs)

type Block = ((Int, Int, Int), (Int, Int, Int))

parseLine :: String -> Block
parseLine line =
    let nonnum = not . isDigit
        takebit (_, s) = head (reads $ dropWhile nonnum s) :: (Int, [Char])
        numbers = map fst $ iterate takebit (0, line)
        rawA = (numbers !! 1, numbers !! 2, numbers !! 3)
        rawB = (numbers !! 4, numbers !! 5, numbers !! 6)
     in (min rawA rawB, max rawA rawB)

doFalling :: [Block] -> (Int, [Block])
doFalling blocks = go groundArray sortedBlocks
  where
    firstTwo (a, b, _) = (a, b)
    f3 f (a, b, c) (d, e, g) = (a `f` d, b `f` e, c `f` g)
    maxdims = foldr (\(a, b) c -> f3 max (f3 max a b) c) (0, 0, 0) blocks
    sortedBlocks = sortOn (\((_, _, z1), (_, _, z2)) -> min z1 z2) blocks
    groundArray = listArray ((0, 0), firstTwo maxdims) (replicate (rangeSize ((0, 0), firstTwo maxdims)) 0)
    go _ [] = (0, [])
    go heights (b@((bx0, by0, bz0), (bx1, by1, bz1)) : bs) =
        let newz = 1 + maximum [heights ! (x', y') | x' <- [bx0 .. bx1], y' <- [by0 .. by1]]
            dropped = bz0 - newz
            b' = ((bx0, by0, bz0 - dropped), (bx1, by1, bz1 - dropped)) :: Block
            heights' =
                accum
                    (const id)
                    heights
                    [ ((x', y'), z')
                    | x' <- [bx0 .. bx1]
                    , y' <- [by0 .. by1]
                    , z' <- [bz1 - dropped]
                    ]
            (finalN, rst) = go heights' bs
         in ((if b == b' then 1 else 0) + finalN, b' : rst)

-- k supports blocks in v
supportGraph :: [Block] -> Map Int [Int]
supportGraph blocks = M.map (nub . sort) $ M.delete 0 $ M.fromListWith (++) deplist
  where
    f3 f (a, b, c) (d, e, g) = (a `f` d, b `f` e, c `f` g)
    maxdims = foldr (\(a, b) c -> f3 max (f3 max a b) c) (0, 0, 0) blocks
    arrayBounds = ((0, 0, 0), maxdims)
    baseArray = listArray arrayBounds (replicate (rangeSize arrayBounds) 0)
    fullArray =
        accum
            (const id)
            baseArray
            [ ((x, y, z), idx)
            | (idx, b) <- zip [1 ..] blocks
            , let ((x0, y0, z0), (x1, y1, z1)) = b
            , x <- [x0 .. x1]
            , y <- [y0 .. y1]
            , z <- [z0 .. z1]
            ]
    deplist =
        [ (fullArray ! (x, y, z - 1), [idx])
        | (idx, b) <- zip [1 ..] blocks
        , let ((x0, y0, z0), (x1, y1, z1)) = b
        , x <- [x0 .. x1]
        , y <- [y0 .. y1]
        , z <- [min z0 z1]
        ]

blockFallCount :: Map Int [Int] -> Map Int (S.Set Int) -> Int -> Int
blockFallCount supporting supportedBy target = S.size (go (S.singleton target) (S.singleton target)) - 1
  where
    go destroyed boundary =
        let newboundarypot = S.fromList (concatMap (flip (M.findWithDefault []) supporting) boundary) S.\\ destroyed
            newboundary = S.filter (\x -> M.findWithDefault S.empty x supportedBy `S.isSubsetOf` destroyed) newboundarypot
         in if S.null boundary then destroyed else go (S.union destroyed newboundary) newboundary

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc22.in" else head args
    s <- lines <$> readFile filename
    let blocks = map parseLine s
    let (_, fallen) = doFalling blocks
    let supporting = supportGraph fallen
    let supportedBy = M.map S.fromList $ M.fromListWith (++) (concatMap (\(x, y) -> map (,[x]) y) $ M.toList supporting)
    let supportCounts = M.map S.size supportedBy
    print $
        length
            [ ()
            | bid <- [1 .. length fallen]
            , let supportList = M.findWithDefault [] bid supporting
            , not (any ((== Just 1) . flip M.lookup supportCounts) supportList)
            ]
    print $ sum $ map (blockFallCount supporting supportedBy) [1 .. length fallen]
