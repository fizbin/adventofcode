import Control.Monad
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Dijkstra (Heap (EmptyH), popH, pushH)
import System.Environment (getArgs)

-- import Debug.Trace

numbers :: String -> [Int]
numbers [] = []
numbers s = case reads s of
    [] -> numbers $ dropWhile (\ch -> ch /= '-' && not (isDigit ch)) (tail s)
    (n, rest) : _ -> n : numbers rest

type Terra x = (x, x, x, x)
data Blueprint = Blueprint
    { bpNum :: Int
    , bpRules :: Terra (Terra Int)
    }

ore :: Terra a -> a
ore (a, _, _, _) = a
clay :: Terra a -> a
clay (_, a, _, _) = a
obsid :: Terra a -> a
obsid (_, _, a, _) = a
geod :: Terra a -> a
geod (_, _, _, a) = a

oneRule :: Terra (Terra Int)
oneRule = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1))

zipWithT :: (a -> b -> c) -> Terra a -> Terra b -> Terra c
zipWithT f (a, b, c, d) (a', b', c', d') = (f a a', f b b', f c c', f d d')
allT :: (a -> Bool) -> Terra a -> Bool
allT f (a, b, c, d) = f a && f b && f c && f d
andT :: Terra Bool -> Bool
andT = allT id

parseLine :: String -> Blueprint
parseLine s = either error id parse
  where
    parse = do
        let nums = numbers s
        let n = head nums
        let [a, b, c, d, e, f] = tail nums
        pure $ Blueprint n ((a, 0, 0, 0), (b, 0, 0, 0), (c, d, 0, 0), (e, 0, f, 0))

-- If you had infinite ore, how many geodes could you get?
-- overestimate this, so imagine building all types of robots in parallel
maxGeods :: Int -> Terra Int -> Terra Int -> Terra (Terra Int) -> Int
maxGeods 0 _ rocks _ = geod rocks
maxGeods timeLeft robots rocks rules =
    let nrocks = zipWithT (+) robots rocks
        oreR = ore robots + 1
        clayR = clay robots + 1
        (obsidR, nrocks') =
            if clay rocks >= clay (obsid rules)
                then (obsid robots + 1, zipWithT (-) nrocks (0, clay (obsid rules), 0, 0))
                else (obsid robots, nrocks)
        (geodR, nrocks'') =
            if obsid rocks >= obsid (geod rules)
                then (geod robots + 1, zipWithT (-) nrocks (0, 0, obsid (geod rules), 0))
                else (geod robots, nrocks')
     in maxGeods (timeLeft - 1) (oreR, clayR, obsidR, geodR) nrocks'' rules

doBlueprint :: Blueprint -> Int -> Int
doBlueprint bp maxT =
    let initialHeap = pushH EmptyH (0, 0, (1, 0, 0, 0), (0, 0, 0, 0))
     in fromMaybe 0 (work initialHeap)
  where
    rules = bpRules bp
    work :: Heap (Int, Int, Terra Int, Terra Int) -> Maybe Int
    work heap = do
        ((_, time, robos, rocks), heap') <- popH heap
        -- traceM $ show (bpNum bp) ++ " " ++ show time ++ " " ++ show npot ++ " " ++ show robos ++ " " ++ show rocks
        if time == maxT
            then pure (geod rocks)
            else do
                let noop = (robos, zipWithT (+) rocks robos)
                let actions = flip mapMaybe [ore, clay, obsid, geod] $ \rocktype ->
                        do
                            guard $ andT $ zipWithT (<=) (rocktype rules) rocks
                            let one = rocktype oneRule
                            let robos' = zipWithT (+) robos one
                            let rocks' = zipWithT (-) (zipWithT (+) robos rocks) (rocktype rules)
                            pure (robos', rocks')
                let hpadd hp (robos', rocks') =
                        let pot = maxGeods (maxT - time - 1) robos' rocks' rules
                         in if pot > 0 then pushH hp (-pot, time + 1, robos', rocks') else hp
                let heap'' = foldl' hpadd heap' (noop : actions)
                work heap''

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc19.in"
                else head args
    s <- lines <$> readFile filename
    let blueprints = parseLine <$> s
    -- part 1
    let part1Results = (\bp -> (bpNum bp, doBlueprint bp 24)) <$> blueprints
    print $ sum $ uncurry (*) <$> part1Results
    -- part 2
    print $ product $ (`doBlueprint` 32) <$> take 3 blueprints
