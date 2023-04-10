{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Arrow (Arrow ((&&&)))
import Control.Monad (join)
import Data.Char (isSpace)
import Data.List (foldl')
import Data.List.Split (splitWhen)
import qualified Data.Map as M
import Data.Maybe (isJust)
import System.Environment (getArgs)

-- import Debug.Trace (trace)

class VecOps x where
    -- | Vector addition
    ($+$) :: x -> x -> x

    -- | Scalar multiplication
    iMul :: Int -> x -> x

infixl 6 $+$

instance (Num a, Num b) => VecOps (a, b) where
    (a, b) $+$ (a', b') = (a + a', b + b')
    iMul x (a, b) = (fromIntegral x * a, fromIntegral x * b)

-- remember, x increases DOWN, and y increases RIGHT

turnRight :: Num a => (a, a) -> (a, a)
turnRight (a, b) = (b, -a)
turnLeft :: Num a => (a, a) -> (a, a)
turnLeft (a, b) = (-b, a)

-- | Parses into (Left False) for "L", (Left True) for "R", or (Right dist) for "walk dist"
parseLine :: String -> [Either Bool Int]
parseLine ('L' : s) = Left False : parseLine s
parseLine ('R' : s) = Left True : parseLine s
parseLine [] = []
parseLine (w : s) | isSpace w = parseLine s
parseLine s = case reads s of
    (r, s') : _ -> Right r : parseLine s'
    [] -> error $ "Out of parses " ++ show s

part1NextSpot :: M.Map (Int, Int) a -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
part1NextSpot grid = nextSpotDir
  where
    nextSpotDir :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
    nextSpotDir spot dir = (nextSpot spot dir, dir)
    nextSpot spot dir =
        let base = spot $+$ dir
            back = (\(a, b) (a', b') -> (a - 300 * a', b - 300 * b')) spot dir
         in if isJust (M.lookup base grid)
                then base
                else head $ filter (isJust . (`M.lookup` grid)) $ iterate ($+$ dir) back

doPart :: ((Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))) -> M.Map (Int, Int) Char -> [Either Bool Int] -> (Int, Int, Int)
doPart nextSpotDir grid directions =
    let initialspot = fst $ M.findMin grid
        initialdir = (0, 1) -- that is, to the right
        (finalspot, finaldir) = foldl' (flip go) (initialspot, initialdir) directions
        (row, col) = finalspot
        fdir = case finaldir of
            (0, 1) -> 0
            (1, 0) -> 1
            (0, -1) -> 2
            (-1, 0) -> 3
            x -> error $ "Bad final dir " ++ show x
     in (row + 1, col + 1, fdir)
  where
    go (Left True) (spot, dir) = (spot, turnRight dir)
    go (Left False) (spot, dir) = (spot, turnLeft dir)
    go (Right dist) (spot, dir) = go' dist spot dir
    go' 0 spot dir = (spot, dir)
    go' dist spot dir =
        let (nxtspot, nxtdir) = nextSpotDir spot dir
         in case M.lookup nxtspot grid of
                Just '#' -> (spot, dir)
                Just '.' -> go' (dist - 1) nxtspot nxtdir
                sth -> error $ "Bad lookup: " ++ show sth

part2NextSpot :: M.Map (Int, Int) a -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
part2NextSpot grid = nextSpotDir
  where
    nextSpotDir spot dir = case M.lookup (spot, dir) joinMap of
        Just (spot', revdir) -> (spot', (-1) `iMul` revdir)
        Nothing -> (spot $+$ dir, dir)
    initialspot = fst $ M.findMin grid
    gridBoundary :: [Either Bool ((Int, Int), (Int, Int))]
    gridBoundary = mkBoundary initialspot initialspot (-1, 0) (0, 1)
    -- boundary goes clockwise and is: (Left True) for a right (3/4 open) turn or (Left False) a left (1/4 open) turn,
    -- or (Right (pt, outdir)) for a square on the boundary.
    -- Basically, (Left False) represents a corner from which one can start joining an edge (because around that corner we have
    -- three squares with stuff in them) and (Left True) represents a corner we can NOT join from.
    mkBoundary stopPoint currPt outDir nxtDir =
        let nxtPt = currPt $+$ nxtDir
            turn :: [Either Bool ((Int, Int), (Int, Int))]
            (turn, nxtPt', outDir', nxtDir') = case (M.lookup nxtPt grid, M.lookup (nxtPt $+$ outDir) grid) of
                (Just _, Nothing) ->
                    -- good, just moving along a flat boundary as normal
                    ([], nxtPt, outDir, nxtDir)
                (Nothing, Nothing) ->
                    -- oops, should turn right
                    ([Left True], currPt, turnRight outDir, turnRight nxtDir)
                (Just _, Just _) ->
                    -- oops, should turn left
                    ([Left False], currPt $+$ nxtDir $+$ outDir, turnLeft outDir, turnLeft nxtDir)
                (Nothing, Just _) ->
                    -- can't happen; implies some grid pattern like:
                    --     s.       or       .s
                    --     .s                s.
                    -- where the "s" are spaces
                    error "Bad turn"
            rest =
                if nxtPt' == stopPoint && nxtDir' == (0, 1)
                    then []
                    else mkBoundary stopPoint nxtPt' outDir' nxtDir'
         in Right (currPt, outDir) : turn ++ rest
    joinMap =
        let joined = joinAll gridBoundary
            joined' = map (snd &&& fst) joined
         in M.fromList $ joined ++ joined'
    joinAll [] = []
    joinAll boundary = case break (== Left False) boundary of
        ([], Left False : _) -> error "We don't join to leave a 'Left False' at the front of boundary"
        (before, Left False : after) -> let (a, b) = joiner (reverse before) after in a ++ joinAll b
        _ -> error ("Somehow we aren't done, but have no corners to join from: " ++ show boundary)

    -- The meat of how edges are joined into a cube. "joiner" is called from "joinAll" with the two sides of "boundary"
    -- that were on either side of a "Left False". It then joins along until it finds a "Left True" on one side or another,
    -- and at that point if it's "Left True" on only one of (lsh, rhs), it inserts a "Left False" at the new corner,
    -- because one "Left True" means that we've ended our join leaving a spot where three corners come together.
    -- If it finds a "Left True" on both lhs and rhs at the same time, it leaves the join "flat", without any
    -- "Left" corner marker.
    joiner ((Left False):_) _ = error "joined *into* an existing join start point"
    joiner _ ((Left False):_) = error "joined *into* an existing join start point"
    joiner [] (Left True : rhs) = ([], rhs)
    joiner [] rhs@(Right _ : _) = ([], rhs)
    joiner ((Left True) : lhs) ((Left True) : rhs) = ([], reverse lhs ++ rhs)
    joiner ((Left True) : lhs) rhs = ([], reverse lhs ++ (Left False : rhs))
    joiner lhs ((Left True) : rhs) = ([], reverse lhs ++ (Left False : rhs))
    joiner (Right a : lhs) (Right b : rhs) = let (rest, leftover) = joiner lhs rhs in ((a, b) : rest, leftover)
    joiner lhs [] = error $ "rhs is guaranteed to have at least one Left in it: " ++ show lhs

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc22.in"
                else head args
    s <- lines <$> readFile filename
    let [grid, directionStr] = splitWhen null s
    let directions = parseLine $ join directionStr
    let gridMap :: M.Map (Int, Int) Char
        gridMap =
            M.fromList $
                map (\(r, (c, d)) -> ((r, c), d)) $
                    filter (\(_, (_, c)) -> not (isSpace c)) $
                        join $
                            zipWith (map . (,)) [0 ..] $
                                map (zip [0 ..]) grid
    let (p1row, p1col, p1dir) = doPart (part1NextSpot gridMap) gridMap directions
    -- print (p1row, p1col, p1dir)
    print $ 1000 * p1row + 4 * p1col + p1dir
    let (p2row, p2col, p2dir) = doPart (part2NextSpot gridMap) gridMap directions
    -- print (p2row, p2col, p2dir)
    print $ 1000 * p2row + 4 * p2col + p2dir
