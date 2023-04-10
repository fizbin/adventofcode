{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad
import qualified Data.Array.IArray as IA
import Data.Array.Unboxed (UArray)
import Dijkstra (aStar')
import System.Environment (getArgs)

-- import Debug.Trace (trace)

neigh :: IA.IArray a Char => Int -> Int -> a (Int, Int) Char -> ((Int, Int), Int) -> [((Int, Int), Int)]
neigh pitchHeight pitchWidth grid ((row, col), t) = do
    (row', col') <- [(row + r, col + c) | (r, c) <- [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)]]
    let sp = (row', col')
    let (low, high) = IA.bounds grid
    -- traceM $ show (low, high)
    guard $ sp >= low
    guard $ sp <= high
    -- let tracedBang arr ind = let (l, h) = IA.bounds arr in if (l <= ind) && (ind <= h) then Right (arr IA.! ind) else Left (show ind ++ " not in " ++ show (l, h))
    guard $ '#' /= grid IA.! sp
    when ((1 <= row) && (row <= pitchHeight)) $ do
        guard $ '<' /= grid IA.! (row', 1 + ((col' + t) `mod` pitchWidth))
        guard $ '>' /= grid IA.! (row', 1 + ((col' - t - 2) `mod` pitchWidth))
        guard $ '^' /= grid IA.! (1 + ((row' + t) `mod` pitchHeight), col')
        guard $ 'v' /= grid IA.! (1 + ((row' - t - 2) `mod` pitchHeight), col')
    pure ((row', col'), t + 1)

estimate :: (Int, Int) -> ((Int, Int), a) -> Int
estimate goal ((row, col), _) = abs (fst goal - row) + abs (snd goal - col)

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc24.in"
                else head args
    s <- lines <$> readFile filename
    let pitchWidth = length (head s) - 2
    let pitchHeight = length s - 2
    let asrc = [((row, col), ch) | (row, line) <- zip [0 ..] s, (col, ch) <- zip [0 ..] line]
    -- traceM $ show asrc
    -- traceM $ show ((0, 0), (pitchHeight + 1, pitchWidth + 1))
    let grid :: UArray (Int, Int) Char
        grid = IA.array ((0, 0), (pitchHeight + 1, pitchWidth + 1)) asrc
    -- print $ grid IA.! (0, 0)
    -- traceM "Got array"
    let start = head $ map ((0,) . fst) $ filter (\(_, ch) -> ch == '.') $ zip [0 ..] (head s)
    let goal = head $ map ((length s - 1,) . fst) $ filter (\(_, ch) -> ch == '.') $ zip [0 ..] (last s)
    -- print goal
    -- print $ dijkstra (map (,1::Int) . neigh pitchHeight pitchWidth grid) (start, 0) ((== goal) . fst)
    let Just part1 = aStar' (map (,1) . neigh pitchHeight pitchWidth grid) (start, 0) (estimate goal) ((== goal) . fst)
    print (snd $ fst part1)

    let Just part2a = aStar' (map (,1) . neigh pitchHeight pitchWidth grid) (fst part1) (estimate start) ((== start) . fst)
    let Just part2b = aStar' (map (,1) . neigh pitchHeight pitchWidth grid) (fst part2a) (estimate goal) ((== goal) . fst)
    print (snd $ fst part2b)
