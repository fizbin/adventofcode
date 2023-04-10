-- stack --resolver lts-18.18 script --package multiset --package containers

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Arrow
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map.Strict as M
--import Debug.Trace
import System.Environment

data DiceImpl m where
  DiceImpl ::
    { roll :: m Int,
      norm :: forall a. (Ord a) => m a -> m a
    } ->
    DiceImpl m

game :: forall m. Monad m => DiceImpl m -> Int -> Int -> Int -> m (Int, Int)
game die goal start1 start2 = turn 0 0 start1 start2 True
  where
    turn score1 score2 p1 p2 isP1Turn =
      if score1 >= goal || score2 >= goal
        then pure (score1, score2)
        else do
          res <- norm die $ sum <$> sequence [roll die, roll die, roll die]
          let p1' = if isP1Turn then ((p1 + res - 1) `mod` 10) + 1 else p1
          let p2' = if isP1Turn then p2 else ((p2 + res - 1) `mod` 10) + 1
          let score1' = if isP1Turn then score1 + p1' else score1
          let score2' = if isP1Turn then score2 else score2 + p2'
          turn score1' score2' p1' p2' $ not isP1Turn

newtype Universe a = Universe {ununi :: [(a, Int)]} deriving (Show)

instance Functor Universe where
  fmap f (Universe uni) = Universe $ map (first f) uni

instance Applicative Universe where
  pure x = Universe [(x, 1)]
  f <*> u = Universe [(f' x, a * b) | (f', a) <- ununi f, (x, b) <- ununi u]

instance Monad Universe where
  u >>= f = Universe [(y, a * b) | (x, a) <- ununi u, (y, b) <- ununi (f x)]

main :: IO ()
main = do
  args <- getArgs
  let filename = maybe "aoc21.in" fst $ uncons args
  datas <- lines <$> readFile filename
  let start1 = read $ tail $ dropWhile (/= ':') (datas !! 0)
  let start2 = read $ tail $ dropWhile (/= ':') (datas !! 1)
  let die1 = DiceImpl (modify (+ 1) >> (\x -> 1 + (x -1 `mod` 100)) <$> get) id 
  let ((p1score, p2score), lastRoll) = runState (game die1 1000 start1 start2) 0
  print $ lastRoll * min p1score p2score
  let normU (Universe uni) = let m = M.toAscList $ M.fromListWith (+) uni in Universe m
  let die2 = DiceImpl (Universe [(1, 1), (2, 1), (3, 1)]) normU
  let unival = game die2 21 start1 start2
  let wonLost = normU $ uncurry (<) <$> unival
  print $ maximum $ map snd $ ununi wonLost