import Data.Map.Strict qualified as M
import Data.Map.Strict(Map)
import Data.Char (isDigit)
import Data.Set qualified as S
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Foldable (Foldable(foldl'))

parseLine :: String -> Maybe (Int, [Int], [Int])
parseLine line = do
        [cardbit, rest] <- pure $ splitOn ":" line
        [winningS, haveS] <- pure $ splitOn "|" rest
        pure (read (dropWhile (not . isDigit) cardbit), map read $ words winningS, map read $ words haveS)

winPoints :: Int -> Int
winPoints 0 = 0
winPoints 1 = 1
winPoints n = 2 * winPoints (n - 1)

main :: IO ()
main = do
        args <- getArgs
        let filename = if null args then "aoc4.in" else head args
        s <- lines <$> readFile filename
        let parsed = map (fromJust . parseLine) s
        let idNwin = map (\(i, w, h) -> (i, S.size $ S.intersection (S.fromList w) (S.fromList h))) parsed
        print $ sum $ map (winPoints.snd) idNwin
        let cardMap0 = M.fromList $ map ((,1::Int) . fst) idNwin :: Map Int Int
        let cardMap :: Map Int Int
            cardMap = foldl' (\m (i, nw) ->
                             let ncopies = m M.! i :: Int
                              in foldl' (flip (M.adjust (ncopies +))) m [i+1..i+nw]) cardMap0 idNwin
        print $ sum $ M.elems cardMap
