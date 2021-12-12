-- stack --resolver nightly-2021-11-28 script --package mtl --package containers
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TupleSections #-}

--import Debug.Trace
import Data.Map (Map, (!))
import qualified Data.Map as M
import System.Environment

isCap :: [Char] -> Bool
isCap (s:_)
  | s <= 'Z' = True
isCap _ = False

true0 :: Num p => Bool -> p -> p
true0 True _ = 0
true0 False x = x

p1 :: Map String [String] -> Int
p1 graph = go "start" []
  where
    addToBeen w been
      | isCap w = been
      | otherwise = w : been
    go "end" _ = 1
    go w been =
      sum $
      map
        (\nxt -> true0 (nxt `elem` been) $ go nxt (addToBeen w been))
        (graph ! w)

p2 :: Map String [String] -> Int
p2 graph = go "start" False []
  where
    addToBeen w been
      | isCap w = been
      | otherwise = w : been
    go "end" _ _ = 1
    -- if we get to start with a non-empty "been" list, don't count it
    go "start" _ (_:_) = 0
    go w True been =
      sum $
      map
        (\nxt -> true0 (nxt `elem` been) $ go nxt True (addToBeen w been))
        (graph ! w)
    go w False been =
      sum $
      map (\nxt -> go nxt (nxt `elem` been) (addToBeen w been)) (graph ! w)

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc12.in"
          else head args
  datas <- words <$> readFile filename
  let graph =
        M.fromListWith (++) $
        concatMap
          (\s ->
             let (a, b) = break (== '-') s
              in [(a, [tail b]), (tail b, [a])])
          datas
  print $ p1 graph
  print $ p2 graph
