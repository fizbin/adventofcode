import Control.Arrow (second)

-- import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List (elemIndex, foldl', sort, sortOn)
import Data.Maybe (fromJust)
import Data.Set qualified as S
import System.Environment (getArgs)

mostFrequent :: (Ord a) => [a] -> (a, Int)
mostFrequent [] = error "empty mostFrquent"
mostFrequent a@(x : _) = go (x, 1) (sort a)
    where
        go sofar [] = sofar
        go sofar (y : ys) =
                let (ally, noty) = break (/= y) ys
                    ycount = 1 + length ally
                    newsofar = if ycount > snd sofar then (y, ycount) else sofar
                 in go newsofar noty

handType1 :: String -> Int
handType1 hand =
        let dcount = S.size $ S.fromList hand
            (_, mfreq) = mostFrequent hand
         in case dcount of
                1 -> 7
                2 -> if mfreq == 4 then 6 else 5
                3 -> if mfreq == 3 then 4 else 3
                4 -> 2
                _ -> 1

handScore1 :: String -> Int
handScore1 hand =
        foldl'
                (\sofar card -> 100 * sofar + fromJust (elemIndex card "023456789TJQKA"))
                (handType1 hand)
                hand

handType2 :: String -> Int
handType2 hand =
        let dcount = S.size $ S.fromList (filter (/= 'J') hand)
            (_, mfreq0) = mostFrequent (filter (/= 'J') hand)
            mfreq = mfreq0 + length (filter (== 'J') hand)
         in case dcount of
                0 -> 7
                1 -> 7
                2 -> if mfreq == 4 then 6 else 5
                3 -> if mfreq == 3 then 4 else 3
                4 -> 2
                _ -> 1

handScore2 :: String -> Int
handScore2 hand =
        foldl'
                (\sofar card -> 100 * sofar + fromJust (elemIndex card "0J23456789TQKA"))
                (handType2 hand)
                hand

main :: IO ()
main = do
        args <- getArgs
        let filename = if null args then "aoc7.in" else head args
        s <- lines <$> readFile filename
        let handPairs :: [(String, Int)]; handPairs = map (second read . break isSpace) s
        let handPairs1 = sortOn (handScore1 . fst) handPairs
        let score1 = (sum $ zipWith (*) [1 ..] (map snd handPairs1)) :: Int
        -- forM_ handPairs1 $ \(hand, _) ->
        --         print (hand, handScore1 hand)
        print score1
        let handPairs2 = sortOn (handScore2 . fst) handPairs
        let score2 = (sum $ zipWith (*) [1 ..] (map snd handPairs2)) :: Int
        print score2
