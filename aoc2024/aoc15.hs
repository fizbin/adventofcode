import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import Data.Map qualified as M
-- import Debug.Trace
import System.Environment (getArgs)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (!x1, !y1) (!x2, !y2) = (x1 + x2, y1 + y2)

getdir :: Char -> (Int, Int)
getdir '^' = (-1, 0)
getdir '>' = (0, 1)
getdir '<' = (0, -1)
getdir 'v' = (1, 0)
getdir _ = error "Bad direction"

doStep :: ((Int, Int), M.Map (Int, Int) Char) -> Char -> ((Int, Int), M.Map (Int, Int) Char)
doStep (roboloc, mymap) dirch =
  let mydir = getdir dirch
      findMoving :: (Int, Int) -> Maybe [(Int, Int)]
      findMoving from =
        let simpleMove = (from :) <$> findMoving (add mydir from)
         in case (mydir, M.findWithDefault '#' from mymap) of
              (_, '#') -> Nothing
              (_, '.') -> Just []
              (_, 'O') -> simpleMove
              (_, '@') -> simpleMove
              ((0, _), '[') -> simpleMove
              ((0, _), ']') -> simpleMove
              (_, '[') ->
                (\b c -> from : add from (0, 1) : b ++ c)
                  <$> findMoving (add mydir from)
                  <*> findMoving (add (add mydir from) (0, 1))
              (_, ']') ->
                (\b c -> from : add from (0, -1) : b ++ c)
                  <$> findMoving (add mydir from)
                  <*> findMoving (add (add mydir from) (0, -1))
              _ -> error "Map confusion"
      moving = findMoving roboloc
   in -- trace (dumpMap mymap ++ "stepping " ++ [dirch]) $
      case moving of
        Nothing -> (roboloc, mymap)
        Just s ->
          ( add roboloc mydir,
            M.fromList
              [(loc `add` mydir, mymap ! loc) | loc <- s]
              `M.union` M.fromList ((,'.') <$> s)
              `M.union` mymap
          )

checkSum :: Map (Int, Int) Char -> Int
checkSum mp = sum $ (\((x, y), _) -> 100 * x + y) <$> filter (\(_, ch) -> ch == 'O' || ch == '[') (M.toList mp)

dumpMap :: Map (Int, Int) Char -> String
dumpMap mp =
  let rowMap = M.fromListWith M.union $ map (\((x, y), char) -> (x, M.singleton y char)) $ M.toList mp
   in concatMap (\(_, row) -> map snd (M.toAscList row) ++ "\n") $ M.toAscList rowMap

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc15.in" else head args
  [mapsec, stepspec] <- splitOn "\n\n" <$> readFile filename
  let grid = lines mapsec
  let mymap = M.fromList $ do
        (x, row) <- zip [0 ..] grid
        (y, ch) <- zip [0 ..] row
        pure ((x, y), ch)
  let roboloc = head $ map fst $ filter (('@' ==) . snd) (M.toList mymap)
  let finalMap = foldl' doStep (roboloc, mymap) (concat $ lines stepspec)
  putStrLn $ "Part 1: " ++ show (checkSum $ snd finalMap)
  let widen ((x, y), '@') = [((x, 2 * y), '@'), ((x, 2 * y + 1), '.')]
      widen ((x, y), 'O') = [((x, 2 * y), '['), ((x, 2 * y + 1), ']')]
      widen ((x, y), chr) = [((x, 2 * y), chr), ((x, 2 * y + 1), chr)]
  let mymap' = M.fromList $ concatMap widen (M.toList mymap)
  let roboloc' = head $ map fst $ filter (('@' ==) . snd) (M.toList mymap')
  let finalMap' = foldl' doStep (roboloc', mymap') (concat $ lines stepspec)
  --   putStrLn $ dumpMap (snd finalMap')
  putStrLn $ "Part 2: " ++ show (checkSum $ snd finalMap')