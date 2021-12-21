-- stack --resolver lts-18.18 script --package multiset --package containers
{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall #-}

import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Arrow

--import Debug.Trace
import System.Environment

import Numeric

readNumber :: Num p => String -> p
readNumber s =
  case readInt 2 (`elem` "#.") pnum s of
    (n, ""):_ -> n
    _ -> error $ "No parse for " ++ show s
  where
    pnum '#' = 1
    pnum _ = 0

doStep :: String -> (Char, Map (Int, Int) Char) -> (Char, Map (Int, Int) Char)
doStep key old@(def, mp) = normalize (key !! readNumber (replicate 9 def), mp')
  where
    oldxs = map fst $ M.keys mp
    oldys = map snd $ M.keys mp
    getNumberFor (x, y) =
      readNumber $ curry (getSpot old) <$> [x - 1 .. x + 1] <*> [y - 1 .. y + 1]
    mp' =
      M.fromList $
      map (id &&& ((key !!) . getNumberFor)) $
      (,) <$> [minimum oldxs - 1 .. maximum oldxs + 1] <*>
      [minimum oldys - 1 .. maximum oldys + 1]

normalize :: Eq a => (a, Map (Int, Int) a) -> (a, Map (Int, Int) a)
normalize (def, mp) = (def, M.filter (/= def) mp)

getSpot :: (a, Map (Int, Int) a) -> (Int, Int) -> a
getSpot (def, mp) = flip (M.findWithDefault def) mp

main :: IO ()
main = do
  args <- getArgs
  let filename = maybe "aoc20.in" fst $ uncons args
  datas <- words <$> readFile filename
  let key = head datas
  let boardstrs = tail datas
  let board =
        ( '.'
        , M.fromList $
          zip
            ((,) <$> [1 .. length boardstrs] <*> [1 .. length (head boardstrs)])
            (concat $ tail datas))
  let doItAll = iterate (doStep key) board
  print $ M.size (snd $ doItAll !! 2)
  print $ M.size (snd $ doItAll !! 50)
