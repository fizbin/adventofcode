import qualified Data.Set as S
import System.Environment (getArgs)

neigh3d (x, y, z) = [(x + 1, y, z), (x - 1, y, z),
                     (x, y + 1, z), (x, y - 1, z),
                     (x, y, z + 1), (x, y, z - 1)]

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc18.in"
                else head args
    s <- lines <$> readFile filename
    let pts :: [(Int, Int, Int)]; pts = map (\ln -> read $ "(" ++ ln ++ ")") s
    let ptSet = S.fromList pts
    print $ length [n | p <- pts, n <- neigh3d p, S.notMember n ptSet]
    let untilEq f x = let y = f x in if x == y then y else untilEq f y
    let allnums = concatMap (\(a, b, c) -> [a, b, c]) pts
    let inbounds =
            let a = minimum allnums
                b = maximum allnums
             in (\(x, y, z) -> a - 2 <= x && x <= b + 2 && a - 2 <= y
                            && y <= b + 2 && a - 2 <= z && z <= b + 2)
    let nbhd pt = neigh3d pt ++ [pt]
    let air = untilEq (S.fromList . filter inbounds . filter (`S.notMember` ptSet) . concatMap nbhd . S.toList)
            $ S.singleton (minimum allnums - 1, minimum allnums, minimum allnums)
    print $ length [n | p <- pts, n <- neigh3d p, n `S.member` air]
