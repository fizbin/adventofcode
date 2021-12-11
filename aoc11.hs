-- stack --resolver nightly-2021-11-28 script --package mtl --package containers
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TupleSections #-}

--import Debug.Trace
import Control.Applicative
import Control.Monad.State.Strict
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc11.in"
          else head args
  datas <- words <$> readFile filename
  let coordsWithNeighbors =
        mkCoordsWithNeighbors (length datas) (length $ head datas)
  let allspots = map (read . (: [])) $ concat datas
  let datamap = M.fromList $ zip (map fst coordsWithNeighbors) allspots
  let coordsWithNeighborsM = M.fromList coordsWithNeighbors
  let flashers =
        tail $
        map fst $ iterate (dostep coordsWithNeighborsM . snd) (0, datamap)
  print $ sum $ take 100 flashers
  print $ fst $ head $ filter ((== 100) . snd) $ zip [1 :: Int ..] flashers

repState :: (a -> State Bool a) -> a -> a
repState action start =
  let (out, keepGoing) = runState (action start) False
   in if keepGoing
        then repState action out
        else out

dostep ::
     Map (Int, Int) [(Int, Int)]
  -> Map (Int, Int) Int
  -> (Int, Map (Int, Int) Int)
dostep neighbors mymp =
  let mbmap = go $ M.map (Just . (+ 1)) mymp
   in (M.size $ M.filter isNothing mbmap, M.map (fromMaybe 0) mbmap)
  where
    bloom :: ((Int, Int), Maybe Int) -> State Bool [((Int, Int), Maybe Int)]
    bloom (c, Just x)
      | x >= 10 =
        put True >> pure ((c, Nothing) : map (, Just 1) (neighbors ! c))
    bloom x = pure [x]
    go :: Map (Int, Int) (Maybe Int) -> Map (Int, Int) (Maybe Int)
    go =
      repState $ \mp ->
        M.fromListWith (liftA2 (+)) . concat <$> mapM bloom (M.toList mp)

mkCoordsWithNeighbors :: Int -> Int -> [((Int, Int), [(Int, Int)])]
mkCoordsWithNeighbors rows cols =
  [ ((i, j), nbs)
  | i <- [0 .. rows - 1]
  , j <- [0 .. cols - 1]
  , nbs <-
      [ [ (p, q)
        | p <- [i - 1, i, i + 1]
        , 0 <= p
        , p < rows
        , q <- [j - 1, j, j + 1]
        , 0 <= q
        , q < cols
        , (p, q) /= (i, j)
        ]
      ]
  ]
