{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import qualified Data.Map as M
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import qualified Data.Set as S
import Data.List
import System.Environment (getArgs)

-- import Debug.Trace (trace)

($+$) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) $+$ (a', b') = (a + a', b + b')

infixl 6 $+$

rotRight :: Num b => (b, a) -> (a, b)
rotRight (a, b) = (b, -a)

doStep :: Int -> S.Set (Int, Int) -> Maybe (S.Set (Int, Int))
doStep tick elves =
    let nxt = S.map (\e -> M.findWithDefault e e proposedMap) elves
     in if nxt == elves then Nothing else Just nxt
  where
    allNeighbors (a, b) = [(a + i, b + j) | i <- [-1, 0, 1], j <- [-1, 0, 1], (i /= 0) || (j /= 0)]
    southNbs = [(1, -1), (1, 0), (1, 1)]
    westNbs = map rotRight southNbs
    northNbs = map rotRight westNbs
    eastNbs = map rotRight northNbs
    potdirs = take 4 $ drop (tick `mod` 4) [northNbs, southNbs, westNbs, eastNbs, northNbs, southNbs, westNbs]
    proposed elf = let allProposed = map (map (elf $+$)) potdirs
                       filteredP = filter (all (`S.notMember` elves)) allProposed
                    in if all (`S.notMember` elves) (allNeighbors elf) then Nothing
                    else head . tail <$> listToMaybe filteredP
    proposedMapSrc = mapMaybe (\e -> (e,) <$> proposed e) $ S.toList elves
    one = 1 :: Int
    valueTimes = M.fromListWith (+) $ map (\(_, b) -> (b, one)) proposedMapSrc
    proposedMap = M.fromList $ filter (\(_, e) -> valueTimes M.! e < 2) proposedMapSrc


main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc23.in"
                else head args
    s <- lines <$> readFile filename
    let elves = S.fromList $ [(r, c) | (r, line) <- zip [0 ..] s, (c, ch) <- zip [0 ..] line, ch == '#']
    -- part 1
    -- let applyN :: Int -> (a->a) -> a -> a; applyN 0 _ x = x; applyN n f x = applyN (n-1) f (f x)
    let tenSteps = foldl' (\e t -> fromMaybe e (doStep t e)) elves [0..9]
    let spanWidth vs = maximum vs - minimum vs + 1
    let area = spanWidth (S.map fst tenSteps) * spanWidth (S.map snd tenSteps)
    print $ area - S.size tenSteps

    -- part 2
    let p2ans n e = maybe (n+1) (p2ans $ n+1) (doStep n e)
    print $ p2ans 0 elves
