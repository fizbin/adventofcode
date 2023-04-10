-- stack --resolver lts-18.18 script --package containers
{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall #-}

import Data.List
import System.Environment

runStep :: [String] -> [String]
runStep orig =
  let triple row =
        [(last row, head row, head $ tail row)] ++
        zip3 row (tail row) (drop 2 row) ++
        [(last $ init row, last row, head row)]
      hmovef ('>', '.', _) = '>'
      hmovef (_, '>', '.') = '.'
      hmovef (_, x, _) = x
      hmove = [map hmovef (triple row) | row <- orig]
      vmovef ('v', '.', _) = 'v'
      vmovef (_, 'v', '.') = '.'
      vmovef (_, s, _) = s
      vmove = transpose [map vmovef (triple row) | row <- transpose hmove]
   in vmove

main :: IO ()
main = do
  args <- getArgs
  let filename = maybe "aoc25.in" fst $ uncons args
  datas <- words <$> readFile filename
  let repf (i, _, nextstate) = (i + 1, nextstate, runStep nextstate)
  let stuff = iterate repf (1 :: Int, datas, runStep datas)
  print $ (\(a, _, _) -> a) $ head $ filter (\(_, b, c) -> b == c) stuff
