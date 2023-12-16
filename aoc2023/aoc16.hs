import Data.Map qualified as M
import Data.Set qualified as S
import System.Environment (getArgs)

handleChar :: Char -> (Int, Int) -> [(Int, Int)]
handleChar ch beamDir
    | ch == '/' = [(-(snd beamDir), -(fst beamDir))]
    | ch == '\\' = [(snd beamDir, fst beamDir)]
    | ch == '|', fst beamDir == 0 = [(-1, 0), (1, 0)]
    | ch == '-', snd beamDir == 0 = [(0, -1), (0, 1)]
    | otherwise = [beamDir]

addC :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addC (a, b) (c, d) = (a + c, b + d)

lighteds :: M.Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> S.Set (Int, Int)
lighteds grid initPos initDir = S.intersection (M.keysSet grid) $ S.map fst $ go S.empty [(initPos, initDir)]
  where
    go sofar [] = sofar
    go sofar beams = go (sofar `S.union` S.fromList beams) (concatMap handleBeam $ filter (`S.notMember` sofar) beams)
    handleBeam (bpos, bdir) = case M.lookup bpos grid of
        Just ch -> map (\d -> (addC bpos d, d)) (handleChar ch bdir)
        Nothing -> []

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc16.in" else head args
    s <- lines <$> readFile filename
    let grid = M.fromList [((i, j), ch) | (i, row) <- zip [0 ..] s, (j, ch) <- zip [0 ..] row]
    print $ S.size $ lighteds grid (0, 0) (0, 1)
    let width = length $ head s
        height = length s
    print $
        maximum $
            map S.size $
                [lighteds grid (r, 0) (0, 1) | r <- [0 .. height - 1]]
                    ++ [lighteds grid (r, width - 1) (0, -1) | r <- [0 .. height - 1]]
                    ++ [lighteds grid (0, c) (1, 0) | c <- [0 .. width - 1]]
                    ++ [lighteds grid (height - 1, c) (-1, 0) | c <- [0 .. width - 1]]
