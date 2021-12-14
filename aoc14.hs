-- stack --resolver nightly-2021-11-28 script --package mtl --package containers
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE Haskell2010 #-}

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as M

--import Debug.Trace
import System.Environment

every :: Int -> [a] -> [a]
every _ [] = []
every n lst = head lst : every n (drop n lst)

dostep :: Map String Char -> (Map String Int, Char) -> (Map String Int, Char)
dostep rules (inner, lastc) =
  (M.fromListWith (+) (concatMap applyRule $ M.toList inner), lastc)
  where
    applyRule (mypair, count) =
      case M.lookup mypair rules of
        Nothing -> [(mypair, count)]
        Just c -> [([head mypair, c], count), ([c, mypair !! 1], count)]

getAns :: (Map String Int, Char) -> Int
getAns (inner, lastC) =
  let cntMap =
        M.fromListWith (+) $ (lastC, 1) : map (first head) (M.toList inner)
      counts = map snd $ M.toList cntMap
   in maximum counts - minimum counts

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc14.in"
          else head args
  datas <- words <$> readFile filename
  let startpoly = head datas
  let rules =
        M.fromList $
        zipWith
          (\a b -> (a, head b))
          (every 3 $ tail datas)
          (every 3 $ drop 3 datas)
  let startMap =
        M.fromListWith (+) $
        zipWith (\a b -> ([a, b], 1)) startpoly (tail startpoly)
  let mapRes = iterate (dostep rules) (startMap, last startpoly)
  print $ map getAns mapRes !! 10
  print $ map getAns mapRes !! 40
