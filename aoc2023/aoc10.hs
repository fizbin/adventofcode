import Control.Monad (guard)
import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Set qualified as S
import System.Environment (getArgs)

pipeDir :: M.Map Char [(Int, Int)]
pipeDir =
        M.fromList
                [ ('|', [(-1, 0), (1, 0)])
                , ('-', [(0, -1), (0, 1)])
                , ('L', [(-1, 0), (0, 1)])
                , ('J', [(-1, 0), (0, -1)])
                , ('7', [(1, 0), (0, -1)])
                , ('F', [(1, 0), (0, 1)])
                , ('.', [])
                , ('S', [(1, 0), (0, 1), (-1, 0), (0, -1)])
                ]

negC :: (Num a, Num b) => (a, b) -> (a, b)
negC (a, b) = (-a, -b)
addC :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addC (a, b) (c, d) = (a + c, b + d)
getC :: [[a]] -> (Int, Int) -> Maybe a
getC x (a, b) =
        if a >= length x
                then Nothing
                else
                        let row = x !! a
                         in if b >= length row then Nothing else Just (row !! b)

findSloop :: [[Char]] -> [(Int, Int)]
findSloop grid =
        let spot = head [(rowidx, colidx) | (rowidx, row) <- zip [0 ..] grid, (colidx, ch) <- zip [0 ..] row, ch == 'S']
            firstDir = head [d | d <- pipeDir M.! 'S', n <- maybeToList (grid `getC` (d `addC` spot)), negC d `elem` (pipeDir M.! n)]
            firstStep = spot `addC` firstDir
         in spot : travel (S.fromList [spot, firstStep]) firstStep
    where
        travel :: S.Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
        travel seen nowAt =
                let nexts :: [(Int, Int)]
                    nexts = do
                        ch <- maybeToList (grid `getC` nowAt)
                        d <- pipeDir M.! ch
                        n <- maybeToList (grid `getC` (d `addC` nowAt))
                        guard $ negC d `elem` (pipeDir M.! n)
                        let p = nowAt `addC` d
                        guard $ p `S.notMember` seen
                        pure p
                 in case nexts of
                        [] -> [nowAt]
                        (n : _) -> nowAt : travel (n `S.insert` seen) n

main :: IO ()
main = do
        args <- getArgs
        let filename = if null args then "aoc10.in" else head args
        grid <- lines <$> readFile filename
        let sLoop = findSloop grid
            sLoopPairs = zip sLoop (tail sLoop ++ [head sLoop])
        print $ length sLoop `div` 2
        let windingP1 = [((i, j), 1) | (von, zu) <- sLoopPairs, zu == (1, 0) `addC` von, i <- [fst zu], j <- [0 .. snd zu - 1]]
            windingP2 = [((i, j), -1) | (von, zu) <- sLoopPairs, zu == (-1, 0) `addC` von, i <- [fst von], j <- [0 .. snd von - 1]]
            windings' = M.fromListWith (+) (windingP1 ++ windingP2) :: M.Map (Int, Int) Int
            windings = foldl' (flip M.delete) windings' sLoop
        print $ length $ filter (/= 0) $ M.elems windings
