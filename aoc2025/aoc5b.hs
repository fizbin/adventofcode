{-# LANGUAGE LambdaCase #-}

import Data.List (foldl', sort)
import System.Environment (getArgs)

newtype IdRange =
  IdRange (Integer, Integer)
  deriving (Eq, Ord, Show)

instance Read IdRange where
  readsPrec _ s =
    [ (IdRange (low, hi), s''')
    | (low, s') <- reads s
    , ('-':s'') <- [s']
    , (hi, s''') <- reads s''
    ]

data Command
  = StartRange
  | IdVal
  | EndRange
  deriving (Eq, Ord)

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc5.in"
          (x:_) -> x
  dataLines <- lines <$> readFile filename
  let (rangeStrs, idStrs) = break null dataLines
  let ranges = map read rangeStrs :: [IdRange]
  let myIds = map read $ words (unwords idStrs)
  let unified =
        sort
          $ map (, IdVal) myIds
              ++ concatMap
                   (\(IdRange (lo, hi)) -> [(lo, StartRange), (hi, EndRange)])
                   ranges
  let ans =
        snd
          $ foldl'
              (\(depth, allowed) ->
                 \case
                   (_, StartRange) -> (depth + 1, allowed)
                   (_, IdVal) ->
                     if depth > 0
                       then (depth, allowed + 1)
                       else (depth, allowed)
                   (_, EndRange) -> (depth - 1, allowed))
              (0 :: Int, 0 :: Int)
              unified
  print ans
  let ans2 =
        (\(_, _, c) -> c)
          $ foldl'
              (\(depth, recentBegin, totSize) ->
                 \case
                   (x, StartRange) ->
                     if depth == 0
                       then (depth + 1, x, totSize)
                       else (depth + 1, recentBegin, totSize)
                   (x, EndRange) ->
                     if depth == 1
                       then (depth - 1, 0, totSize + x - recentBegin + 1)
                       else (depth - 1, recentBegin, totSize)
                   (_, IdVal) -> (depth, recentBegin, totSize))
              (0 :: Int, 0 :: Integer, 0 :: Integer)
              unified
  print ans2
