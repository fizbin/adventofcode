{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
import System.Environment(getArgs)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.List
import Control.Monad.State.Strict

findCombos :: [String] -> String -> Int
findCombos towels target = evalState (doCalc target) M.empty
  where
    doCalc :: String -> State (M.Map String Int) Int
    doCalc "" = pure 1
    doCalc tgt = do
      prev <- gets (M.lookup tgt)
      case prev of
        Just ans -> pure ans
        Nothing -> do
          subAns <- forM (filter (`isPrefixOf` tgt) towels) $
            \towel -> doCalc (drop (length towel) tgt)
          let ans = sum subAns
          modify (M.insert tgt ans)
          pure ans


main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc19.in" else head args
  (towelspec, patspec) <- splitOn "\n\n" <$> readFile filename >>= \case
     [a, b] -> pure (a, b)
     _ -> ioError (userError "Bad file structure; expected two paragraphs")
  let towels = splitOn ", " towelspec
  let combos = findCombos towels <$> lines patspec
  print $ length $ filter (/= 0) combos
  print $ sum combos
