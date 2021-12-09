{-# LANGUAGE Haskell2010 #-}

import Data.List (sort)
import Data.Maybe
import System.Environment

-- isSub a b asks "is 'a' a sublist of 'b'?" and assumes 'a' and 'b' are sorted
isSub :: Ord a => [a] -> [a] -> Bool
isSub [] _ = True
isSub _ [] = False
isSub (a:as) (b:bs)
  | a < b = False
  | a == b = isSub as bs
  | a > b = isSub (a : as) bs
  | otherwise = error "Trichotomy!"

-- i.e. "lookup" backwards
pukool :: Eq a => a -> [(b, a)] -> Maybe b
pukool a bs = lookup a $ map (\(x, y) -> (y, x)) bs

p2 :: ([String], [String]) -> Int
p2 (ins, outs) =
  let known1 =
        [(inthing, 1) | inthing <- ins, length inthing == 2] ++
        [(inthing, 7) | inthing <- ins, length inthing == 3] ++
        [(inthing, 4) | inthing <- ins, length inthing == 4] ++
        [(inthing, 8) | inthing <- ins, length inthing == 7]
      known2 =
        [ (inthing, 6)
        | inthing <- ins
        , length inthing == 6
        , Just True /= ((`isSub` inthing) <$> pukool 1 known1)
        ] ++
        [ (inthing, 3)
        | inthing <- ins
        , length inthing == 5
        , Just True == ((`isSub` inthing) <$> pukool 1 known1)
        ] ++
        known1
      known3 =
        [ (inthing, 9)
        | inthing <- ins
        , length inthing == 6
        , Just True == ((`isSub` inthing) <$> pukool 3 known2)
        ] ++
        known2
      known4 =
        [ (inthing, 5)
        | inthing <- ins
        , length inthing == 5
        , isNothing (lookup inthing known3)
        , Just True == ((inthing `isSub`) <$> pukool 9 known3)
        ] ++
        known3
      known5 =
        [ (inthing, 2)
        | inthing <- ins
        , length inthing == 5
        , isNothing (lookup inthing known4)
        ] ++
        [ (inthing, 0)
        | inthing <- ins
        , length inthing == 6
        , isNothing (lookup inthing known4)
        ] ++
        known4 :: [(String, Int)]
   in fromJust $ do
        ints <- mapM (`lookup` known5) outs
        let intstr = concatMap show ints
        pure $ read intstr

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc8.in"
          else head args
  mylines <- lines <$> readFile filename
  let parsem puzzle =
        let ws = words puzzle
            ins = map sort $ take 10 ws
            outs = map sort $ drop 11 ws
         in if null ws
              then Nothing
              else Just (ins, outs)
  let puzzles = mapMaybe parsem mylines
  let p1 =
        length $
        filter (\a -> length a `elem` [2, 4, 3, 7]) $ concatMap snd puzzles
  print p1
  let vals = map p2 puzzles
  print $ sum vals
