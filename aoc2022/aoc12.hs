{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)

import Dijkstra (dijkstra)

neighbors :: Bool -> [String] -> (Int, Int) -> [(Int, Int)]
neighbors isPart1 grid (row, col) =
    let allposs = [(row - 1, col), (row, col - 1), (row + 1, col), (row, col + 1)]
        allpos' = filter ((>= 0) . snd) $ filter ((>= 0) . fst) allposs
        allpos'' = filter ((< length grid) . fst) $ filter ((< (length $ head grid)) . snd) allpos'
        toelev 'S' = 'a'
        toelev 'E' = 'z'
        toelev x = x
        celev (r, c) = toelev ((grid !! r) !! c)
        nowelev = celev (row, col)
     in if isPart1
            then [p | p <- allpos'', celev p <= succ nowelev]
            else [p | p <- allpos'', celev p >= pred nowelev]

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc12.in"
                else head args
    s <- lines <$> readFile filename
    let getSpot (r, c) = (s !! r) !! c
    let startrow = head $ filter (\r -> 'S' `elem` (s !! r)) [0 .. (length s - 1)]
    let startcol = head $ filter (\c -> 'S' == getSpot (startrow, c)) [0 .. (length (head s) - 1)]
    let addOneInt =
            let one = 1 :: Int
             in map (, one)
    let Just (final, dist) =
            dijkstra (addOneInt . neighbors True s) (startrow, startcol) (\p -> 'E' == getSpot p)
    print dist
    let Just (_, dist') =
            dijkstra (addOneInt . neighbors False s) final (\p -> getSpot p `elem` "aS")
    print dist'
