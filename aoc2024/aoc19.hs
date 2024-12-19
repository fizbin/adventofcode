{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

import Data.List (inits, tails)
import Data.List.Split (splitOn)
import Data.Set qualified as S
import System.Environment (getArgs)

findCombos :: S.Set String -> String -> Int
findCombos towels target = caf !! targetl
  where
    -- (caf !! k) is the number of ways to make (take k target)
    caf = map findAns $ inits target
    targetl = length target
    findAns "" = 1
    findAns tgt =
      sum [caf !! clen | (t, clen) <- zip (tails tgt) [0 ..], t `S.member` towels]

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc19.in" else head args
  (towelspec, patspec) <-
    splitOn "\n\n" <$> readFile filename >>= \case
      [a, b] -> pure (a, b)
      _ -> ioError (userError "Bad file structure; expected two paragraphs")
  let towels = S.fromList $ splitOn ", " towelspec
  let combos = findCombos towels <$> lines patspec
  print $ length $ filter (/= 0) combos
  print $ sum combos
