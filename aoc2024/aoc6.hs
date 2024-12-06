{-# LANGUAGE BangPatterns #-}

import Control.Arrow
import Control.Monad (guard, when)
import Control.Monad.State (put, runState)
import Control.Monad.Trans (lift)
import Data.HashMap.Strict qualified as DH
import Data.Maybe (isNothing)
import Data.Set qualified as S
import ListT qualified as L
import System.Environment (getArgs)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (!a, !b) (!c, !d) = (a + c, b + d)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (a, b) = (b, -a)

guardStep :: DH.HashMap (Int, Int) Char -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
guardStep cmap (guardspot, guarddir) =
  case DH.lookup (add guardspot guarddir) cmap of
    Just '#' -> guardStep cmap (guardspot, turnRight guarddir)
    _ -> (add guardspot guarddir, guarddir)

guardRun :: DH.HashMap (Int, Int) Char -> ((Int, Int), (Int, Int)) -> Maybe [(Int, Int)]
guardRun !cmap = guardRun' S.empty
  where
    guardRun' !hist state | state `S.member` hist = Nothing
    guardRun' !hist state = case DH.lookup (fst state) cmap of
      Nothing -> Just []
      Just _ -> (fst state :) <$> guardRun' (S.insert state hist) (guardStep cmap state)

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc6.in" else head args
  grid <- lines <$> readFile filename
  let (cmap, guardStart) = first DH.fromList $ flip runState (0, 0) $ L.toList $ do
        (row, line) <- L.fromFoldable $ zip [0 ..] grid
        (col, char) <- L.fromFoldable $ zip [0 ..] line
        when (char == '^') $ lift $ put (row, col)
        pure ((row, col), char)
  let guardhist = S.toList $ S.fromList $ case guardRun cmap (guardStart, (-1, 0)) of
        Just foo -> foo
        Nothing -> error "Initial scenario stuck in loop"
  putStrLn $ "Part 1: " ++ show (length guardhist)
  let obstacleLocs = do
        obstacle <- guardhist
        guard $ obstacle /= guardStart
        guard $ isNothing $ guardRun (DH.insert obstacle '#' cmap) (guardStart, (-1, 0))
        pure obstacle
  putStrLn $ "Part 2: " ++ show (length obstacleLocs)
