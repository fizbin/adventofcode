{-# LANGUAGE RecordWildCards #-}

import Data.Array
import Data.List (foldl')
import Data.Map qualified as M
import Dijkstra qualified
import System.Environment (getArgs)

addC :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addC (a, b) (c, d) = (a + c, b + d)

data State = State {stPos :: (Int, Int), stDir :: (Int, Int), stInARow :: Int} deriving (Eq, Ord, Show)

turnLeft :: (Num b) => (b, b) -> (b, b)
turnLeft (x, y) = (-y, x)
turnRight :: (Num b) => (b, b) -> (b, b)
turnRight (x, y) = (y, -x)

transition :: Int -> Int -> ((Int, Int) -> Maybe Int) -> State -> [(State, Int)]
transition minRun maxRun grid (State{..}) =
    let potd =
            [(False, turnLeft stDir) | stInARow >= minRun]
                ++ [(False, turnRight stDir) | stInARow >= minRun]
                ++ ([(True, stDir) | stInARow < maxRun])
     in (`concatMap` potd) $ \(sd, d) ->
            let newp = d `addC` stPos
             in case grid newp of
                    Nothing -> []
                    Just cst -> [(State{stPos = newp, stDir = d, stInARow = if sd then 1 + stInARow else 1}, cst)]

findMinDist :: M.Map (Int, Int) Int -> (Int, Int) -> Int -> M.Map (Int, Int) Int
findMinDist grid iPos iVal =
    go M.empty [(iPos, iVal)]
  where
    go ans [] = ans
    go ans vals =
        let (ans', nvals) = foldl' mknvals (ans, []) vals
         in go ans' nvals
    mknvals (ans, newSoFar) (pos, val) =
        let pval = M.lookup pos ans
            hasNew = maybe True (> val) pval
            newPos =
                [ (fst pos, snd pos + 1)
                , (fst pos, snd pos - 1)
                , (fst pos + 1, snd pos)
                , (fst pos - 1, snd pos)
                ]
         in if hasNew
                then case M.lookup pos grid of
                    Nothing -> (ans, newSoFar)
                    Just v -> (M.insert pos val ans, ((,val + v) <$> newPos) ++ newSoFar)
                else (ans, newSoFar)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc17.in" else head args
    s <- lines <$> readFile filename
    let grid = M.fromList [((i, j), read [ch]) | (i, row) <- zip [0 ..] s, (j, ch) <- zip [0 ..] row]
        width = length $ head s
        height = length s
    let minGrid = findMinDist grid (height - 1, width - 1) 0
    let aBounds = ((0, 0), (height - 1, width - 1))
        gridA = listArray aBounds [grid M.! spot | spot <- range aBounds]
        minGridA = listArray aBounds [minGrid M.! spot | spot <- range aBounds]
        gridLookup spot = if inRange aBounds spot then Just (gridA ! spot) else Nothing
    let ans1 =
            Dijkstra.dijkstraGenN'
                (transition 1 3 gridLookup)
                [(State (0, 0) (0, 1) 0, 0), (State (0, 0) (1, 0) 0, 0)]
                (+)
                ((minGridA !) . stPos)
                ((== (height - 1, width - 1)) . stPos)
    print $ snd <$> ans1

    let ans2 =
            Dijkstra.dijkstraGenN'
                (transition 4 10 gridLookup)
                [(State (0, 0) (0, 1) 0, 0), (State (0, 0) (1, 0) 0, 0)]
                (+)
                ((minGridA !) . stPos)
                (\st -> (stPos st == (height - 1, width - 1)) && (stInARow st >= 4))
    print $ snd <$> ans2
