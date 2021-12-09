{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import Data.List (sort)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc7.in" else head args
  strs <- words <$> readFile filename
  let states = read ("[" ++ head strs ++ "]") :: [Int]
  let mina = minimum states
  let maxa = maximum states
  print $ minimum [sum [abs (x - spot) | x <- states] | spot <- [mina..maxa]]
  let sq x = (x*(x+1)) `div` 2
  print $ minimum [sum [sq(abs (x - spot)) | x <- states] | spot <- [mina..maxa]]
