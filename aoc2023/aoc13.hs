import Data.List (transpose)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

-- number of imperfections in reflecting about a horizontal mirror with rtop rows above the mirror
imperfections :: [[Char]] -> Int -> Int
imperfections grid rtop =
    let rsize = min rtop (length grid - rtop)
     in length
            [ ()
            | rdist <- [0 .. rsize - 1]
            , let idx1 = rtop - rdist - 1
            , let idx2 = rtop + rdist
            , (tc, bc) <- zip (grid !! idx1) (grid !! idx2)
            , tc /= bc
            ]

findVertReflect :: Int -> [[Char]] -> Int
findVertReflect imperfectionTarget grid =
    let ansList = [rtop | rtop <- [1 .. length grid - 1], imperfectionTarget == imperfections grid rtop]
     in case ansList of
            [] -> 0
            [a] -> a
            _ -> error $ "answer list: " ++ show ansList

findHorizReflect :: Int -> [[Char]] -> Int
findHorizReflect imperfectionTarget grid =
    let grid' = transpose grid
        ansList = [rtop | rtop <- [1 .. length grid' - 1], imperfectionTarget == imperfections grid' rtop]
     in case ansList of
            [] -> 0
            [a] -> a
            _ -> error $ "answer list: " ++ show ansList

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc13.in" else head args
    paras <- splitOn "\n\n" <$> readFile filename
    let grids = map lines paras
    print $ sum (map (findHorizReflect 0) grids) + 100 * sum (map (findVertReflect 0) grids)
    print $ sum (map (findHorizReflect 1) grids) + 100 * sum (map (findVertReflect 1) grids)
