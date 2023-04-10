#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE Haskell2010 #-}

import Data.List

--import Debug.Trace
import System.Environment

score1 :: [Char] -> Int
score1 = go []
  where
    go ss ('[':xs) = go (']' : ss) xs
    go ss ('{':xs) = go ('}' : ss) xs
    go ss ('<':xs) = go ('>' : ss) xs
    go ss ('(':xs) = go (')' : ss) xs
    go (s:ss) (x:xs)
      | x == s = go ss xs
    go _ [] = 0
    go _ (')':_) = 3
    go _ (']':_) = 57
    go _ ('}':_) = 1197
    go _ ('>':_) = 25137
    go s e = error $ "Huh? " ++ show s ++ "  " ++ show e

score2 :: [Char] -> Int
score2 line =
  let finalstack = go [] line
   in foldl' (\prev x -> prev * 5 + scorec x) 0 finalstack
  where
    go ss ('[':xs) = go (']' : ss) xs
    go ss ('{':xs) = go ('}' : ss) xs
    go ss ('<':xs) = go ('>' : ss) xs
    go ss ('(':xs) = go (')' : ss) xs
    go (s:ss) (x:xs)
      | x == s = go ss xs
    go s [] = s
    go _ (x:_)
      | x `elem` "])}>" = []
    go s e = error $ "Huh? " ++ show s ++ "  " ++ show e
    scorec ')' = 1
    scorec ']' = 2
    scorec '}' = 3
    scorec '>' = 4
    scorec x = error $ "Can't score " ++ show x

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc10.in"
          else head args
  datas <- words <$> readFile filename
  print $ sum $ map score1 datas
  let scores = sort $ filter (/= 0) $ map score2 datas
  -- print scores
  print $ scores !! (length scores `div` 2)
