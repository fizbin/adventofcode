import Control.Monad (foldM, forM)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State qualified as ST
import Data.Foldable (foldrM)
import Data.List (zip4)
import Data.Set qualified as S
import System.Environment (getArgs)

-- import Debug.Trace
part1 :: [String] -> Int
part1 grid =
  flip ST.execState 0 $ foldM doRow S.empty grid
  where
    doRow :: S.Set Int -> String -> ST.State Int (S.Set Int)
    doRow tachySet row = do
      let newTachys1 = [s | (s, 'S') <- zip [0 ..] row]
      newTachys2 <-
        forM (S.toList tachySet) $ \t ->
          if row !! t == '^'
            then ST.modify' (+ 1) >> pure [t - 1, t + 1]
            else pure [t]
      pure $ S.fromList $ newTachys1 ++ concat newTachys2

part2 :: [String] -> Int
part2 [] = error "Empty input"
part2 grid@(g : _) = case runExcept (foldrM doRow (map (const 1) g) grid) of
  Left x -> x
  Right _ -> error "S not found"
  where
    doRow :: [Char] -> [Int] -> Except Int [Int]
    doRow row combos = mapM doSpot (zip4 row combos (0 : combos) (drop 1 combos ++ [0]))
    doSpot :: (Char, Int, Int, Int) -> Except Int Int
    doSpot ('.', x, _, _) = pure x
    doSpot ('S', x, _, _) = throwError x
    doSpot ('^', _, l, r) = pure $ l + r
    doSpot arg = error $ "doSpot " ++ show arg

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc7.in"
          (x : _) -> x
  dataLines <- lines <$> readFile filename
  print $ part1 dataLines
  print $ part2 dataLines
