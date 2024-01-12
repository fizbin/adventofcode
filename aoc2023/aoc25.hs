{-# LANGUAGE BangPatterns #-}

import Data.List (foldl', sort)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Tuple (swap)
import System.Environment (getArgs)

parseLine :: String -> (String, [String])
parseLine str =
    let (von, rst) = break (== ':') str
     in (von, words $ tail rst)

pathFromTo :: M.Map String [String] -> String -> String -> [(String, String)] -> Maybe [String]
pathFromTo graph from to avoiding = findit $ paths (S.singleton from) [[from]]
  where
    findit (allx@(x : _) : xs) = if x == to then Just (reverse allx) else findit xs
    findit _ = Nothing
    avoidingS = S.fromList avoiding `S.union` S.fromList (map swap avoiding)
    paths :: S.Set String -> [[String]] -> [[String]]
    paths _ [] = []
    paths visitedS sofar = sofar ++ paths visitedS' newpaths
      where
        (visitedS', newpaths) = foldl' foldlaccfn (visitedS, []) sofar
        foldlaccfn (v, acc) [] = (v, acc)
        foldlaccfn (v, acc) allp@(von : _) =
            let nexts = [n | n <- M.findWithDefault [] von graph, n `S.notMember` v, (von, n) `S.notMember` avoidingS]
                nextSet = S.fromList nexts
             in (v `S.union` nextSet, map (: allp) nexts ++ acc)

checkThreePaths :: M.Map String [String] -> String -> String -> Maybe [(String, String)]
checkThreePaths graph from to =
    either Just (const Nothing) $ do
        let avoiding1 = [(from, to)]
        path1 <- case pathFromTo graph from to avoiding1 of
            Nothing -> Left avoiding1
            Just pth -> pure pth
        let avoiding2 = zip path1 (tail path1) ++ avoiding1
        path2 <- case pathFromTo graph from to avoiding2 of
            Nothing -> Left avoiding2
            Just pth -> pure pth
        let avoiding3 = zip path2 (tail path2) ++ avoiding2
        case pathFromTo graph from to avoiding3 of
            Nothing -> Left avoiding3
            Just _ -> pure ()

nodeRegionSizes :: M.Map String [String] -> [(String, String)] -> [Int]
nodeRegionSizes graph avoiding = sort $ M.elems $ M.fromListWith (+) $ map (,1) $ M.elems regionMap
  where
    avoidingS = S.fromList avoiding `S.union` S.fromList (map swap avoiding)
    regionMap :: M.Map String String
    regionMap = mkRegionMap (M.mapWithKey const graph) (M.keys graph)
    mkRegionMap final [] = final
    mkRegionMap wip changed =
        let wipS =
                M.fromListWith min $
                    concat
                        [ [(nb, M.findWithDefault n n wip), (n, M.findWithDefault nb nb wip)]
                        | n <- changed
                        , nb <- M.findWithDefault [] n graph
                        , (n, nb) `S.notMember` avoidingS
                        ]
            wip' = M.unionWith min wip wipS
            changed' = [n | n <- M.keys wipS, M.lookup n wip /= M.lookup n wip']
         in mkRegionMap wip' changed'

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc25.in" else head args
    s <- map parseLine . lines <$> readFile filename
    let graph = M.fromListWith (++) $ concatMap (\(x, y) -> (x, y) : map (,[x]) y) s
    let edges = concatMap (\(x, y) -> map (x,) y) s
    let findWeakEdges _ [] = []
        findWeakEdges !potential ((f, t) : es)
            | (f, t) `S.notMember` potential = findWeakEdges potential es
            | otherwise = case checkThreePaths graph f t of
                Nothing -> findWeakEdges potential es
                Just others ->
                    let newpot = potential `S.intersection` (S.fromList others `S.union` S.fromList (map swap others))
                     in (f, t) : findWeakEdges newpot es
    let weakEdges = findWeakEdges (S.fromList edges `S.union` S.fromList (map swap edges)) edges
    let nodeRegions = nodeRegionSizes graph weakEdges
    print $ product nodeRegions
