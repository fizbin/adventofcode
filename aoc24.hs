{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad
import qualified Data.Map as M
import System.Environment (getArgs)
import Dijkstra
-- import Debug.Trace (trace)

neigh :: Int -> Int -> M.Map (Int, Int) Char -> ((Int, Int), Int) -> [((Int, Int), Int)]
neigh pitchHeight pitchWidth grid ((row, col), t) = do
    (row', col') <- [(row + r, col + c) | (r, c) <- [(0, 0), (-1, 0), (1, 0), (0, -1), (0, 1)]]
    guard $ '#' /= M.findWithDefault '#' (row, col) grid
    when ((1 <= row) && (row <= pitchHeight)) $ do
        guard $ '<' /= grid M.! (row', 1 + ((col' + t) `mod` pitchWidth))
        guard $ '>' /= grid M.! (row', 1 + ((col' - t - 2) `mod` pitchWidth))
        guard $ '^' /= grid M.! (1 + ((row' + t) `mod` pitchHeight), col')
        guard $ 'v' /= grid M.! (1 + ((row' - t - 2) `mod` pitchHeight), col')
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
    let grid = M.fromList [((row, col), ch) | (row, line) <- zip [0 ..] s, (col, ch) <- zip [0 ..] line]
    let start = head $ map ((0,) . fst) $ filter (\(_, ch) -> ch == '.') $ zip [0 ..] (head s)
    let goal = head $ map ((length s - 1,) . fst) $ filter (\(_, ch) -> ch == '.') $ zip [0 ..] (last s)
    let pitchWidth = length (head s) - 2
    let pitchHeight = length s - 2
    -- print goal
    -- print $ dijkstra (map (,1::Int) . neigh pitchHeight pitchWidth grid) (start, 0) ((== goal) . fst)
    let Just part1 = aStar' (map (,1) . neigh pitchHeight pitchWidth grid) (start, 0) (estimate goal) ((== goal) . fst)
    print (snd $ fst part1)

    let Just part2a = aStar' (map (,1) . neigh pitchHeight pitchWidth grid) (fst part1) (estimate start) ((== start) . fst)
    let Just part2b = aStar' (map (,1) . neigh pitchHeight pitchWidth grid) (fst part2a) (estimate goal) ((== goal) . fst)
    print (snd $ fst part2b)
