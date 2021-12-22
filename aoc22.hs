-- stack --resolver lts-18.18 script --package multiset --package containers
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

--import Debug.Trace
import Data.Char
import Data.List
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment

-- both bounds are *inclusive*
data RSol =
  RSol
    { rsX :: (Int, Int)
    , rsY :: (Int, Int)
    , rsZ :: (Int, Int)
    }
  deriving (Show, Eq)

splitX :: Int -> RSol -> (RSol, RSol)
splitX lim RSol {..}
  | fst rsX < lim && lim <= snd rsX =
    (RSol {rsX = (fst rsX, lim - 1), ..}, RSol {rsX = (lim, snd rsX), ..})
splitX a b = error $ "splitX " ++ show (a, b)

splitY :: Int -> RSol -> (RSol, RSol)
splitY lim RSol {..}
  | fst rsY < lim && lim <= snd rsY =
    (RSol {rsY = (fst rsY, lim - 1), ..}, RSol {rsY = (lim, snd rsY), ..})
splitY a b = error $ "splitY " ++ show (a, b)

splitZ :: Int -> RSol -> (RSol, RSol)
splitZ lim RSol {..}
  | fst rsZ < lim && lim <= snd rsZ =
    (RSol {rsZ = (fst rsZ, lim - 1), ..}, RSol {rsZ = (lim, snd rsZ), ..})
splitZ a b = error $ "splitZ " ++ show (a, b)

disjointI :: (Int, Int) -> (Int, Int) -> Bool
disjointI (a, b) (c, d) = (b < c) || (d < a)

disjoint :: RSol -> RSol -> Bool
disjoint (RSol xb1 yb1 zb1) (RSol xb2 yb2 zb2) =
  disjointI xb1 xb2 || disjointI yb1 yb2 || disjointI zb1 zb2

volume :: RSol -> Int
volume RSol {..} = product $ map (\bnd -> 1 + snd bnd - fst bnd) [rsX, rsY, rsZ]

subsetI :: (Int, Int) -> (Int, Int) -> Bool
subsetI (a, b) (c, d) = (c <= a) && (b <= d)

subset :: RSol -> RSol -> Bool
subset (RSol xb1 yb1 zb1) (RSol xb2 yb2 zb2) =
  subsetI xb1 xb2 && subsetI yb1 yb2 && subsetI zb1 zb2

-- The idea is that 'applyAction' maintains a list of non-intersecting blocks
applyAction :: Bool -> RSol -> [RSol] -> [RSol]
applyAction False _ [] = []
applyAction True blk [] = [blk]
applyAction tf ctrl (blk:blks)
  | disjoint ctrl blk = blk : applyAction tf ctrl blks
applyAction tf ctrl (blk:blks)
  | blk `subset` ctrl = applyAction tf ctrl blks
-- so now ctrl and blk intersect, and blk is not a subset of ctrl
applyAction True ctrl (blk:blks)
  | ctrl `subset` blk = blk : blks
applyAction tf ctrl (blk:blks)
  | fst (rsX blk) < fst (rsX ctrl) =
    let (lft, rgt) = splitX (fst (rsX ctrl)) blk
     in lft : applyAction tf ctrl (rgt : blks)
applyAction tf ctrl (blk:blks)
  | fst (rsY blk) < fst (rsY ctrl) =
    let (lft, rgt) = splitY (fst (rsY ctrl)) blk
     in lft : applyAction tf ctrl (rgt : blks)
applyAction tf ctrl (blk:blks)
  | fst (rsZ blk) < fst (rsZ ctrl) =
    let (lft, rgt) = splitZ (fst (rsZ ctrl)) blk
     in lft : applyAction tf ctrl (rgt : blks)
applyAction tf ctrl (blk:blks)
  | snd (rsX blk) > snd (rsX ctrl) =
    let (lft, rgt) = splitX (snd (rsX ctrl) + 1) blk
     in rgt : applyAction tf ctrl (lft : blks)
applyAction tf ctrl (blk:blks)
  | snd (rsY blk) > snd (rsY ctrl) =
    let (lft, rgt) = splitY (snd (rsY ctrl) + 1) blk
     in rgt : applyAction tf ctrl (lft : blks)
applyAction tf ctrl (blk:blks)
  | snd (rsZ blk) > snd (rsZ ctrl) =
    let (lft, rgt) = splitZ (snd (rsZ ctrl) + 1) blk
     in rgt : applyAction tf ctrl (lft : blks)
applyAction tf ctrl (blk:_) = error ("Out of choices: " ++ show (tf, ctrl, blk))

parseInLine :: String -> Maybe (Bool, RSol)
parseInLine inLine =
  listToMaybe
    [ (switch, RSol xb yb zb)
    | (switch, s') <- readSwitch inLine
    , (_, s'2) <- require "x=" s'
    , (xb, s'3) <- readBounds s'2
    , (_, s'4) <- require ",y=" s'3
    , (yb, s'5) <- readBounds s'4
    , (_, s'6) <- require ",z=" s'5
    , (zb, s'7) <- readBounds s'6
    , "" <- [dropWhile isSpace s'7]
    ]
  where
    readSwitch ('o':'n':' ':s) = [(True, s)]
    readSwitch ('o':'f':'f':' ':s) = [(False, s)]
    readSwitch _ = []
    require pref s = [((), drop (length pref) s) | pref `isPrefixOf` s]
    readBounds s =
      [ ((a, b), s''')
      | (a, s') <- reads s
      , (_, s'') <- require ".." s'
      , (b, s''') <- reads s''
      ]

main :: IO ()
main = do
  args <- getArgs
  let filename = maybe "aoc22.in" fst $ uncons args
  datas <- lines <$> readFile filename
  let actions =
        map
          (\s -> fromMaybe (error ("Bad input line: " ++ s)) (parseInLine s))
          datas
  let smallRSol = RSol (-51, 51) (-51, 51) (-51, 50)
  let smallActions = filter (\(_, rs) -> rs `subset` smallRSol) actions
  --print $ length actions
  --print $ length smallActions
  let smallRS =
        foldl' (\blks (tf, ctrl) -> applyAction tf ctrl blks) [] smallActions
  print $ sum $ map volume smallRS
  let allRS = foldl' (\blks (tf, ctrl) -> applyAction tf ctrl blks) [] actions
  print $ sum $ map volume allRS
