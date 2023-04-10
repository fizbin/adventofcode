import Control.Applicative (Alternative (empty))
import Control.Monad
import Data.Char (isDigit)
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace (trace)
import System.Environment (getArgs)

-- import Debug.Trace (trace)

numbers :: String -> [Int]
numbers [] = []
numbers s = case reads s of
    [] -> numbers $ dropWhile (\ch -> ch /= '-' && not (isDigit ch)) (tail s)
    (n, rest) : _ -> n : numbers rest

parseLine :: String -> (String, Int, [String])
parseLine s =
    let ws = words s
        name = head $ tail ws
        [rate] = numbers s
        [_, rest] = splitOn "to valv" s
        places = splitOn ", " $ tail $ dropWhile (/= ' ') rest
     in (name, rate, places)

doRun :: M.Map (String, String) Int -> M.Map String Int -> Int -> M.Map (Set String) Int
doRun paths rates timeleft =
    M.fromListWith max $ [(open, flow) | (t', _, _, open, flow) <- go (timeleft, "AA", 0, S.empty, 0), t' == 0]
  where
    pathRate = M.fromListWith (++) $ map (\((f, t), d) -> (f, [(t, d, r) | let r = M.findWithDefault 0 t rates, r > 0])) $ M.toList paths
    go tup@(t, _, _, _, _) | t <= 0 = []
    go (t, loc, flowrate, open, flow) =
        let moveval =
                [ (t', loc', flowrate + r, loc' `S.insert` open, flow + (d + 1) * flowrate)
                | (loc', d, r) <- pathRate M.! loc
                , let t' = t - d - 1
                , loc' `S.notMember` open
                , t' >= 0
                ]
            sitval = [(0, loc, flowrate, open, flow + t * flowrate)]
            rval = moveval ++ sitval
         in --  trace (show (t, loc, flowrate, open, flow) ++ " -> " ++ show rval) $
            rval ++ concatMap go rval

mkPathMap :: M.Map String [String] -> M.Map (String, String) Int
mkPathMap direct = foldl' go initialMap allLocs
  where
    allLocs = M.keys direct
    initialMap = M.fromList $ [((f, f), 0) | f <- allLocs] ++ [((f, t), 1) | (f, ts) <- M.toList direct, t <- ts]
    go sofar intermed = M.unionWith min sofar $ M.fromListWith min $ do
        f <- allLocs
        t <- allLocs
        case (M.lookup (f, intermed) sofar, M.lookup (intermed, t) sofar) of
            (Just d1, Just d2) -> pure ((f, t), d1 + d2)
            _ -> empty

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc16.in"
                else head args
    s <- lines <$> readFile filename
    let parsed = parseLine <$> s
    let rates = M.fromList $ map (\(a, b, _) -> (a, b)) parsed
    let paths = mkPathMap $ M.fromList $ map (\(a, _, b) -> (a, b)) parsed
    let p1Map = doRun paths rates 30
    print $ maximum p1Map

    let p2MapByOpen = doRun paths rates 26
    let allAnswers = do
            ((key1, val1) : kv2s) <- init $ tails $ M.toList p2MapByOpen
            (key2, val2) <- kv2s
            guard (S.disjoint key1 key2)
            -- guard (S.null $ key1 `S.intersection` key2)
            pure $ val1 + val2
    print $ maximum allAnswers
