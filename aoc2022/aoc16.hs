import Control.Monad
import Data.Char (isDigit)
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
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

doMinute :: M.Map String [String] -> M.Map String Int -> M.Map (String, Set String) Int -> M.Map (String, Set String) Int
doMinute paths rates currstate =
    M.unionWith max updatedFlows $ M.fromListWith max $ newDests ++ newOpens
  where
    updatedFlows :: M.Map (String, Set String) Int
    updatedFlows = M.mapWithKey (\(_, open) n -> n + sum (S.map (rates M.!) open)) currstate
    newDests = do
        ((loc, open), n) <- M.toList updatedFlows
        dest <- paths M.! loc
        pure ((dest, open), n)
    newOpens =
        [ ((loc, S.insert loc open), n)
        | ((loc, open), n) <- M.toList updatedFlows
        , loc `S.notMember` open
        , (rates M.! loc) > 0
        ]

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
    let paths = M.fromList $ map (\(a, _, b) -> (a, b)) parsed
    let p1Map = iterate (doMinute paths rates) (M.singleton ("AA", S.empty) 0) !! 30
    print $ maximum p1Map
    let p2Map = iterate (doMinute paths rates) (M.singleton ("AA", S.empty) 0) !! 26
    let p2MapByOpen = M.fromListWith max $ map (\((_, b), n) -> (b, n)) $ M.toList p2Map
    let allAnswers = do
            ((key1, val1) : kv2s) <- init $ tails $ M.toList p2MapByOpen
            (key2, val2) <- kv2s
            guard (S.null $ key1 `S.intersection` key2)
            pure $ val1 + val2
    print $ maximum allAnswers
