{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

import Data.List
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S
import System.Environment (getArgs)
import qualified Data.Array.IArray as IA
import Data.Array.Unboxed (UArray)

-- import Debug.Trace (trace)

($+$) :: Num a => (a, a) -> (a, a) -> (a, a)
(a, b) $+$ (a', b') = (a + a', b + b')

infixl 6 $+$

rotRight :: Num b => (b, a) -> (a, b)
rotRight (a, b) = (b, -a)

southNbs :: [(Int, Int)]
westNbs :: [(Int, Int)]
northNbs :: [(Int, Int)]
eastNbs :: [(Int, Int)]
southNbs = [(1, -1), (1, 0), (1, 1)]
westNbs = map rotRight southNbs
northNbs = map rotRight westNbs
eastNbs = map rotRight northNbs

allNeighbors :: (Int, Int) -> [(Int, Int)]
allNeighbors (a, b) = [(a + i, b + j) | i <- [-1, 0, 1], j <- [-1, 0, 1], (i /= 0) || (j /= 0)]

doStep :: Int -> S.Set (Int, Int) -> Maybe (S.Set (Int, Int))
doStep tick elves =
    let nxt = foldr S.delete crowded proposedMapRev `S.union` lonely `S.union` M.keysSet proposedMapRev
     in if nxt == elves then Nothing else Just nxt
  where
    arange =
        foldr
            ( \(elfx, elfy) ((minx, miny), (maxx, maxy)) ->
                ((min minx (elfx - 1), min miny (elfy - 1)), (max maxx (elfx + 1), max maxy (elfy + 1)))
            )
            ((0, 0), (0, 0))
            elves
    elfArray = IA.accumArray (const id) True arange [(e, False) | e <- S.toList elves] :: UArray (Int, Int) Bool
    blankspot coord = elfArray IA.! coord
    potdirs = take 4 $ drop (tick `mod` 4) [northNbs, southNbs, westNbs, eastNbs, northNbs, southNbs, westNbs]
    (lonely, crowded) = S.partition (all blankspot . allNeighbors) elves
    proposed elf =
        let allProposed = map (map (elf $+$)) potdirs
            filteredP = filter (all blankspot) allProposed
         in head . tail <$> listToMaybe filteredP
    proposedMapSrc = mapMaybe (\e -> (, Just e) <$> proposed e) $ S.toList crowded
    -- dest -> src
    proposedMapRev = M.mapMaybe id $ M.fromListWith (const $ const Nothing) proposedMapSrc

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
    let tenSteps = foldl' (\e t -> fromMaybe e (doStep t e)) elves [0 .. 9]
    let spanWidth vs = maximum vs - minimum vs + 1
    let area = spanWidth (S.map fst tenSteps) * spanWidth (S.map snd tenSteps)
    print $ area - S.size tenSteps

    -- part 2
    let p2ans n e = maybe (n + 1) (p2ans $ n + 1) (doStep n e)
    print $ p2ans 0 elves
