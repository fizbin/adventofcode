-- stack --resolver lts-18.18 script --package multiset --package containers
{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Maybe
import Data.Semigroup
import System.Environment

dostep :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
dostep (xpos, ypos, xvol, yvol) =
  let xpos' = xpos + xvol
      ypos' = ypos + yvol
      xvol'
        | xvol > 0 = xvol - 1
        | xvol < 0 = xvol + 1
        | otherwise = 0
      yvol' = yvol - 1
   in (xpos', ypos', xvol', yvol')

tallest :: (Int, Int) -> (Int, Int) -> Int -> Int -> Maybe (Max Int)
tallest (targetxmin, targetxmax) (targetymin, targetymax) xvol0 yvol0 =
  go (0, 0, xvol0, yvol0) (Max 0)
  where
    go (_, y, _, yvol) _
      | y < targetymin && yvol < 0 = Nothing
    go (x, y, _, _) h
      | targetxmin <= x && x <= targetxmax && targetymin <= y && y <= targetymax =
        Just h
    go s h =
      let s' = dostep s
          (_, y', _, _) = s'
          h' = h <> Max y'
       in go s' h'

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc17.in"
          else head args
  datas <- words <$> readFile filename
  let notdig = (`notElem` "-0123456789")
  let isdig = (`elem` "-0123456789")
  let xd1 = dropWhile notdig $ datas !! 2
  let yd1 = dropWhile notdig $ datas !! 3
  let xd2 = dropWhile notdig $ dropWhile isdig xd1
  let yd2 = dropWhile notdig $ dropWhile isdig yd1
  let xmin = fst $ head $ reads xd1
  let xmax = fst $ head $ reads xd2
  let ymin = fst $ head $ reads yd1
  let ymax = fst $ head $ reads yd2
  let isqrt s =
        let r = sqrt (fromIntegral s) :: Double
         in floor r
  let allposs =
        tallest (xmin, xmax) (ymin, ymax) <$> [isqrt (2 * xmin) - 1 .. xmax + 1] <*>
        [ymin - 1 .. negate ymin + 1]
  print $ mconcat allposs
  print $ length $ filter isJust allposs
