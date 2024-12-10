import Data.Set qualified as S
import System.Environment (getArgs)

combineWithNbs :: (a -> a -> a -> a -> a -> b) -> a -> [[a]] -> [[b]]
combineWithNbs combiner edge initg = nrows
  where
    edgerow = replicate (length $ head initg) edge
    nrows = zipWith3 rowCombiner (edgerow : init initg) initg (tail initg ++ [edgerow])
    bumpMid mid = zip3 (edge : init mid) mid (tail mid ++ [edge])
    rowCombiner above mid below = zipWith3 spotCombiner above (bumpMid mid) below
    spotCombiner up (left, spot, right) down = combiner up left spot right down

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc10.in" else head args
  grid <- lines <$> readFile filename
  let ngrid = map (map (read . (: []))) grid :: [[Int]]
  let withcoords = do
        (rowidx, row) <- zip [(0 :: Int) ..] ngrid
        pure $ do
          (colidx, spot) <- zip [(0 :: Int) ..] row
          pure ((rowidx, colidx), spot)
  let reachInitFunc (c, s) = if s == 9 then (s, S.singleton c) else (s, S.empty)
  let reach = map (map reachInitFunc) withcoords
  let combiner1 up left mid right down =
        let getit (s', rch) = if s' == fst mid + 1 then rch else S.empty
         in (fst mid, S.unions $ snd mid : (getit <$> [up, left, right, down]))
  let final1 = iterate (combineWithNbs combiner1 (999, S.empty)) reach !! 9
  let part1 = sum $ map (S.size . snd) $ concatMap (filter ((== 0) . fst)) final1
  putStrLn $ "Part 1: " ++ show part1
  let combiner2 _ _ (9, _) _ _ = (9, 1 :: Int)
      combiner2 up left mid right down =
        let getit (s', rch) = if s' == fst mid + 1 then rch else 0
         in (fst mid, sum $ getit <$> [up, left, right, down])
  let paths = map (map (,0)) ngrid
  -- one extra time to get 9 spots correct initially
  let final2 = iterate (combineWithNbs combiner2 (999, 0)) paths !! 10
  let part2 = sum $ map snd $ concatMap (filter ((== 0) . fst)) final2
  putStrLn $ "Part 2: " ++ show part2
