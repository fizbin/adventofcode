import qualified Data.Set as S
import System.Environment (getArgs)

neigh3d :: (Int, Int, Int) -> [(Int, Int, Int)]
neigh3d (x, y, z) =
    [ (x + 1, y, z)
    , (x - 1, y, z)
    , (x, y + 1, z)
    , (x, y - 1, z)
    , (x, y, z + 1)
    , (x, y, z - 1)
    ]

main :: IO ()
main = do
    -- parse input
    args <- getArgs
    let filename =
            if null args
                then "aoc18.in"
                else head args
    s <- lines <$> readFile filename
    let pts = map (\ln -> read $ "(" ++ ln ++ ")") s

    -- part 1
    let ptSet = S.fromList pts
    print $ length [n | p <- pts, n <- neigh3d p, S.notMember n ptSet]

    -- part 2
    let untilEq f x = let y = f x in if x == y then y else untilEq f y
    let allnums = concatMap (\(a, b, c) -> [a, b, c]) pts
    let inbounds =
            let a = minimum allnums
                b = maximum allnums
             in ( \(x, y, z) ->
                       a - 1 <= x && x <= b + 1
                    && a - 1 <= y && y <= b + 1
                    && a - 1 <= z && z <= b + 1
                )
    let nbhd pt = neigh3d pt ++ [pt]
    -- Experimentally, using two points as a start is much faster than using one.
    -- Adding more starting points though seems to slow things down.
    let air =
            untilEq (S.fromList . filter inbounds . filter (`S.notMember` ptSet) . concatMap nbhd . S.toList) $
                S.fromList
                    [ (minimum allnums - 1, minimum allnums, minimum allnums)
                    , (maximum allnums + 1, maximum allnums, maximum allnums)
                    ]
    print $ length [n | p <- pts, n <- neigh3d p, n `S.member` air]
