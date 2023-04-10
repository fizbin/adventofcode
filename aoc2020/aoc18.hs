module Main where

import System.Environment (getArgs)
import Data.Char (isDigit)
-- import Debug.Trace

lookForOp :: Bool -> String -> (Int -> Int, String)
lookForOp isPart2 (' ':s) = lookForOp isPart2 s
lookForOp _ (')':s) = (id, s)
lookForOp isPart2 ('+':s) = let (nxtnum, s') = lookForNum isPart2 s
                                (nxtfn, s'') = lookForOp isPart2 s'
                            in (\x1 -> nxtfn (x1 + nxtnum), s'')
lookForOp isPart2 ('*':s) = let (nxtnum, s') = lookForNum isPart2 s
                                (nxtfn, s'') = lookForOp isPart2 s'
                            in if isPart2
                               then (\x1 -> x1 * nxtfn nxtnum, s'')
                               else (\x1 -> nxtfn (x1 * nxtnum), s'')

lookForNum :: Bool -> String -> (Int, String)
lookForNum isPart2 (' ':s) = lookForNum isPart2 s
lookForNum _ inp@(d:_) | isDigit d = head $ reads inp
lookForNum isPart2 ('(':s) = parseInsideParen isPart2 s

parseInsideParen :: Bool -> String -> (Int, String)
parseInsideParen isPart2 s = let (fstnum, s') = lookForNum isPart2 s
                                 (fstfn, s'') = lookForOp isPart2 s'
                             in (fstfn fstnum, s'')

doProblem1 :: String -> Int
doProblem1 s = fst $ parseInsideParen False (s ++ ")")

doProblem2 :: String -> Int
doProblem2 s = fst $ parseInsideParen True (s ++ ")")

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc18.in" else head args
  s <- readFile filename
  print $ sum $ map doProblem1 $ lines s
  print $ sum $ map doProblem2 $ lines s
