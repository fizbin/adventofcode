{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Arrow
import qualified Data.Set as S
import System.Environment (getArgs)
-- import Debug.Trace

parseLine :: String -> [(Int, Int)]
parseLine line = do
    (start, line') <- onept line
    go start line'
  where
    onept s = do
        (x, ',' : s') <- reads s
        (y, s'') <- reads s'
        pure ((x, y), s'')
    min2max a b = [min a b .. max a b]
    go :: (Int, Int) -> String -> [(Int, Int)]
    go _ [] = []
    go start s = do
        (' ' : '-' : '>' : s') <- [s]
        (next, s'') <- onept s'
        let mypts :: [(Int, Int)]
            mypts =
                if fst next == fst start
                    then map (fst next,) $ min2max (snd start) (snd next)
                    else map (,snd next) $ min2max (fst start) (fst next)
        mypts ++ go next s''

dropSand :: Int -> S.Set (Int, Int) -> (S.Set (Int, Int), Bool)
dropSand maxy grid = case go (500, 0) of
    Just finalsand -> (S.insert finalsand grid, False)
    Nothing -> (grid, True)
  where
    go :: (Int, Int) -> Maybe (Int, Int)
    go spot | snd spot > maxy = Nothing
    go spot =
        let [t1, t2, t3] = map ($ spot) [second succ, pred *** succ, succ *** succ]
            tspot sp = if sp `S.member` grid then Nothing else Just sp
            nspot = tspot t1 <|> tspot t2 <|> tspot t3
         in case nspot of
                Nothing -> Just spot
                Just next -> go next

dropAllSand :: Int -> S.Set (Int, Int) -> S.Set (Int, Int)
dropAllSand maxy grid0 = case dropSand maxy grid0 of
    (grid1, True) -> grid1
    (grid1, False) -> dropAllSand maxy grid1

dropAllSand' :: Int -> S.Set (Int, Int) -> S.Set (Int, Int)
dropAllSand' _ grid0 | (500, 0) `S.member` grid0 = grid0
dropAllSand' maxy grid0 = case dropSand maxy grid0 of
    (_, True) -> error "Bottom bar not long enough"
    (grid1, False) -> dropAllSand' maxy grid1

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc14.in"
                else head args
    s <- lines <$> readFile filename
    -- print $ concatMap parseLine s
    let parsed = concatMap parseLine s
    let maxy = maximum $ map snd parsed
    let grid1 = S.fromList parsed
    let finalGrid = dropAllSand maxy grid1
    print $ S.size finalGrid - S.size grid1
    let grid2 = grid1 <> S.fromList (map (,maxy + 2) [-3000 .. 3000])
    let finalGrid' = dropAllSand' (maxy + 3) grid2
    print $ S.size finalGrid' - S.size grid2
