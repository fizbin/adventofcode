import Control.Arrow
import Control.Monad (guard, when)
import Control.Monad.State (put, runState)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import ListT qualified as L
import System.Environment (getArgs)
import Data.Maybe (isNothing)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a, b) = (a +) *** (b +)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (a, b) = (b, -a)

guardStep :: Map (Int, Int) Char -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
guardStep cmap (guardspot, guarddir) =
  case M.lookup (add guardspot guarddir) cmap of
    Just '#' -> guardStep cmap (guardspot, turnRight guarddir)
    _ -> (add guardspot guarddir, guarddir)

guardRun :: Map (Int, Int) Char -> ((Int, Int), (Int, Int)) -> Maybe [(Int, Int)]
guardRun cmap = guardRun' S.empty
  where
    guardRun' hist state | state `S.member` hist = Nothing
    guardRun' hist state = case M.lookup (fst state) cmap of
      Nothing -> Just []
      Just _ -> (fst state :) <$> guardRun' (S.insert state hist) (guardStep cmap state)

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc6.in" else head args
  grid <- lines <$> readFile filename
  let (cmap, guardStart) = first M.fromList $ flip runState (0, 0) $ L.toList $ do
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
        guard $ isNothing $ guardRun (M.insert obstacle '#' cmap) (guardStart, (-1, 0))
        pure obstacle
  putStrLn $ "Part 2: " ++ show (length obstacleLocs)
