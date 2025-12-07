{-# LANGUAGE LambdaCase #-}

import Control.Monad (foldM, forM)
import Control.Monad.State qualified as ST
import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import System.Environment (getArgs)

-- import Debug.Trace
part1 :: [String] -> Int
part1 grid =
  flip ST.execState 0 $ do
    foldM doRow S.empty grid
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
part2 grid = sum $ M.elems $ foldl' doRow M.empty grid
  where
    doRow :: M.Map Int Int -> String -> M.Map Int Int
    doRow tachySet row = do
      let newTachys1 = [(s, 1) | (s, 'S') <- zip [0 ..] row]
          newTachys2 =
            flip map (M.toList tachySet) $ \(t, mult) ->
              if row !! t == '^'
                then [(t - 1, mult), (t + 1, mult)]
                else [(t, mult)]
      M.fromListWith (+) $ newTachys1 ++ concat newTachys2

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc7.in"
          (x:_) -> x
  dataLines <- lines <$> readFile filename
  print $ part1 dataLines
  print $ part2 dataLines
