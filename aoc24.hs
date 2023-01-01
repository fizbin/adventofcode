{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Array (Array)
import qualified Data.Array.IArray as IA
import Data.Array.Unboxed (UArray)
import qualified Data.Set as S
import System.Environment (getArgs)

-- import Debug.Trace (trace)

getHorizOkays :: (IA.IArray a Char, IA.IArray b (S.Set (Int, Int))) => a (Int, Int) Char -> b Int (S.Set (Int, Int))
getHorizOkays grid =
    let (boundx, boundy) = snd $ IA.bounds grid
        pitchWidth = boundy - 1
        tophole = head $ filter ((== '.') . (grid IA.!)) $ (0,) <$> [0 .. boundy]
        bothole = head $ filter ((== '.') . (grid IA.!)) $ (boundx,) <$> [0 .. boundy]
     in IA.listArray (0, pitchWidth - 1) $
            map
                ( \t ->
                    S.fromList $
                        [tophole, bothole]
                            ++ [ (row, col)
                               | row <- [1 .. boundx - 1]
                               , col <- [1 .. boundy - 1]
                               , '<' /= grid IA.! (row, 1 + ((col + t) `mod` pitchWidth))
                               , '>' /= grid IA.! (row, 1 + ((col - t - 2) `mod` pitchWidth))
                               ]
                )
                [0 .. pitchWidth - 1]

getVertOkays :: (IA.IArray a Char, IA.IArray b (S.Set (Int, Int))) => a (Int, Int) Char -> b Int (S.Set (Int, Int))
getVertOkays grid =
    let (boundx, boundy) = snd $ IA.bounds grid
        pitchHeight = boundx - 1
        tophole = head $ filter ((== '.') . (grid IA.!)) $ (0,) <$> [0 .. boundy]
        bothole = head $ filter ((== '.') . (grid IA.!)) $ (boundx,) <$> [0 .. boundy]
     in IA.listArray (0, pitchHeight - 1) $
            map
                ( \t ->
                    S.fromList $
                        [tophole, bothole]
                            ++ [ (row, col)
                               | row <- [1 .. boundx - 1]
                               , col <- [1 .. boundy - 1]
                               , '^' /= grid IA.! (1 + ((row + t) `mod` pitchHeight), col)
                               , 'v' /= grid IA.! (1 + ((row - t - 2) `mod` pitchHeight), col)
                               ]
                )
                [0 .. pitchHeight - 1]

step :: (IA.IArray b (S.Set (Int, Int))) => b Int (S.Set (Int, Int)) -> b Int (S.Set (Int, Int)) -> Int -> S.Set (Int, Int) -> S.Set (Int, Int)
step horizOkays vertOkays t src =
    let pitchWidth = 1 + snd (IA.bounds horizOkays)
        pitchHeight = 1 + snd (IA.bounds vertOkays)
        horizOk = horizOkays IA.! (t `mod` pitchWidth)
        vertOk = vertOkays IA.! (t `mod` pitchHeight)
        srcUp = S.mapMonotonic (\(r, c) -> (r - 1, c)) src
        srcDn = S.mapMonotonic (\(r, c) -> (r + 1, c)) src
        srcLt = S.mapMonotonic (\(r, c) -> (r, c - 1)) src
        srcRt = S.mapMonotonic (\(r, c) -> (r, c + 1)) src
     in horizOk `S.intersection` vertOk `S.intersection` S.unions [src, srcUp, srcDn, srcLt, srcRt]

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
    let grid :: UArray (Int, Int) Char
        grid = IA.array ((0, 0), (pitchHeight + 1, pitchWidth + 1)) asrc
        tophole = head $ filter ((== '.') . (grid IA.!)) $ (0,) <$> [0 .. pitchWidth + 1]
        bothole = head $ filter ((== '.') . (grid IA.!)) $ (pitchHeight + 1,) <$> [0 .. pitchWidth + 1]
        horizOkays = getHorizOkays grid :: Array Int (S.Set (Int, Int))
        vertOkays = getVertOkays grid
        getTime t startSet goal = if goal `S.member` startSet then t else getTime (t + 1) (step horizOkays vertOkays t startSet) goal

    let part1 = getTime 0 (S.singleton tophole) bothole
    print part1
    let part2a = getTime part1 (S.singleton bothole) tophole
    let part2b = getTime part2a (S.singleton tophole) bothole
    print part2b
