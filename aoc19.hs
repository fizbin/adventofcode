-- stack --resolver lts-18.18 script --package multiset --package containers
{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall #-}

import Data.List

import Data.Char (isDigit)
import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad

--import Debug.Trace
import Data.Foldable (asum)
import System.Environment

type Point = (Int, Int, Int)

newtype DiffPoint =
  DiffPoint
    { unDiff :: Point
    }
  deriving (Show, Eq, Ord)

parseDatas :: String -> [[Point]]
parseDatas s
  | 's' `notElem` s = []
parseDatas s =
  let (_, crest) = break (== 's') s
      (core, rest) = break (== 's') (tail crest)
   in handleCore core : parseDatas rest
  where
    handleCore core =
      let corelines = filter (any isDigit) $ lines (dropWhile (/= '-') core)
       in map (\ln -> read $ "(" ++ ln ++ ")") corelines

rotations :: [Point -> Point]
rotations = ((.) <$> negates1 <*> shuffles1) ++ ((.) <$> negates2 <*> shuffles2)
  where
    -- negate an even number of axes
    negates1 =
      [ id
      , \(x, y, z) -> (x, -y, -z)
      , \(x, y, z) -> (-x, y, -z)
      , \(x, y, z) -> (-x, -y, z)
      ]
    -- even permutations of axes
    shuffles1 = [id, \(x, y, z) -> (y, z, x), \(x, y, z) -> (z, x, y)]
    -- negate an odd number of axes
    negates2 =
      [ \(x, y, z) -> (-x, -y, -z)
      , \(x, y, z) -> (-x, y, z)
      , \(x, y, z) -> (x, -y, z)
      , \(x, y, z) -> (x, y, -z)
      ]
    -- odd permutations of axes
    shuffles2 =
      [ \(x, y, z) -> (y, x, z)
      , \(x, y, z) -> (x, z, y)
      , \(x, y, z) -> (z, y, x)
      ]

rotdiff :: (Point -> Point) -> DiffPoint -> DiffPoint
rotdiff rot dpoint = DiffPoint $ rot $ unDiff dpoint

pairwiseDiffs :: [Point] -> [DiffPoint]
pairwiseDiffs ins =
  [ DiffPoint (a1 - a2, b1 - b2, c1 - c2)
  | (a1, b1, c1) <- ins
  , (a2, b2, c2) <- ins
  , (a1, b1, c1) /= (a2, b2, c2)
  ]

applyMove :: DiffPoint -> [Point] -> [Point]
applyMove (DiffPoint (a, b, c)) = map (\(x, y, z) -> (x + a, y + b, z + c))

matchScanner ::
     [(Set Point, Set DiffPoint)]
  -> [Point]
  -> Maybe ([(Set Point, Set DiffPoint)], DiffPoint)
matchScanner collective scanner = do
  asum $ tryRotation <$> rotations <*> collective
  where
    scdiffs = pairwiseDiffs scanner
    tryRotation ::
         (Point -> Point)
      -> (Set Point, Set DiffPoint)
      -> Maybe ([(Set Point, Set DiffPoint)], DiffPoint)
    tryRotation rot (normedP, normedD) = do
      let rotdiffs = S.fromList $ map (rotdiff rot) scdiffs
          normedDiffs = normedD
          rotisize = S.size (S.intersection rotdiffs normedDiffs)
          rotscanner = map rot scanner
      guard (rotisize > 100)
      --trace (show rotisize) (pure ())
      let runPoint (sx, sy, sz) (cx, cy, cz) = do
            let move = DiffPoint (cx - sx, cy - sy, cz - sz)
                applied = applyMove move rotscanner
                goalSize = S.size normedP + length rotscanner - 12
                merged = foldr S.insert normedP applied
            guard (goalSize >= S.size merged)
            pure ((S.fromList applied, rotdiffs) : collective, move)
      asum $ runPoint <$> rotscanner <*> S.toList normedP

runMatch :: [[Point]] -> (Set Point, [DiffPoint])
runMatch scanners =
  let icollective = S.fromList (head scanners)
      idiff = S.fromList $ pairwiseDiffs (head scanners)
   in go [(icollective, idiff)] [DiffPoint (0, 0, 0)] (tail scanners)
  where
    go collective moves [] = (S.unions $ map fst collective, moves)
    go collective moves remainingScanners =
      case go' collective remainingScanners [] of
        Just (c', m, r') -> go c' (m : moves) r'
        Nothing -> error "Couldn't match all scanners"
    go' _ [] _ = Nothing
    go' collective (sc:scs) viewedScanners =
      case matchScanner collective sc of
        Nothing -> go' collective scs (sc : viewedScanners)
        Just (c', m) -> Just (c', m, reverse viewedScanners ++ scs)

main :: IO ()
main = do
  args <- getArgs
  let filename = maybe "aoc19.in" fst $ uncons args
  scannerdatas <- parseDatas <$> readFile filename
  let (collective, moves) = runMatch scannerdatas
  print $ S.size collective
  print $
    maximum $
    (\(DiffPoint (a, b, c)) (DiffPoint (x, y, z)) ->
       abs (a - x) + abs (b - y) + abs (c - z)) <$>
    moves <*>
    moves
  --print $ S.toAscList collective
