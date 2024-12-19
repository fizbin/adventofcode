{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

findCombos :: [String] -> String -> Int
findCombos towels target = caf !! targetl
  where
    caf = 1 : map findAns [1 ..]
    targetl = length target
    findAns n =
      let tgtTail = drop (targetl - n) target
          poss = [caf !! (n - length t) | t <- towels, t `isPrefixOf` tgtTail]
       in sum poss

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc19.in" else head args
  (towelspec, patspec) <-
    splitOn "\n\n" <$> readFile filename >>= \case
      [a, b] -> pure (a, b)
      _ -> ioError (userError "Bad file structure; expected two paragraphs")
  let towels = splitOn ", " towelspec
  let combos = findCombos towels <$> lines patspec
  print $ length $ filter (/= 0) combos
  print $ sum combos
