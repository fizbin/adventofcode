{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Complex.Generic

import Data.String.Utils
import Debug.Trace

data Tile = Tile {
  _tile_id :: Int,
  _tile_data :: [String],
  _tile_edges :: (String, String, String, String) -- top, right, bottom, left
  }
type Edge = String

instance Ord (Complex Int) where
  compare a b = compare (rect a) (rect b)

data PuzzleHolder = PH {
  _ph_map :: M.Map (Complex Int) Tile,
  _ph_edges :: M.Map Edge (Complex Int, Complex Int)
  }

rotateTile :: Tile -> Tile -- rotate right
rotateTile tile =
  let (top, right, bottom, left) = _tile_edges tile
      new_data = map reverse $ transpose $ _tile_data tile
  in Tile (_tile_id tile) new_data (left, top, right, bottom)

flipTile :: Tile -> Tile -- flip about vertical axis
flipTile tile =
  let (top, right, bottom, left) = _tile_edges tile
  in Tile (_tile_id tile) (map reverse $ _tile_data tile)
     (reverse top, reverse left, reverse bottom, reverse right)

placeTile :: PuzzleHolder -> Tile -> Maybe PuzzleHolder
placeTile holder tile =
  (do
      let (top, right, bottom, left) = _tile_edges tile
      (newloc, newdir) <- M.lookup (_ph_edges holder) top
      

isqrt 0 = 0
isqrt 1 = 1
isqrt n = findit 0
  where
    findit i = if (i+1)*(i+1) <= n then findit (i+1) else i

parseTiles :: String -> [Tile]
parseTiles s = repeatedParse (lines s)
  where
    repeatedParse [] = []
    repeatedParse ("":rest) = repeatedParse rest
    repeatedParse tilelines =
      let (infoline:thistile, rest) = break null tilelines
          tilenum = fst $ head $ reads (dropWhile (/= ' ') infoline)
          mytile = Tile tilenum (strip (concat thistile))
      in mytile : repeatedParse rest

-- rotates one to the right
rotateSquare :: String -> String
rotateSquare s =
  let width = isqrt (length s)
  in
    if length s /= width * width then error "Non-square rotate"
    else concatMap (
      \row -> map (\col -> s !! (width * (width - 1 - col) + row))
              [0..width-1])
         [0..width-1]

-- flips left-to-right
flipSquare :: String -> String
flipSquare s =
  let width = isqrt (length s)
  in
    if length s /= width * width then error "Non-square flip"
    else concatMap (
      \row -> map (\col -> s !! (width * row + width - 1 - col))
              [0..width-1])
         [0..width-1]

-- edges are starting from top, clockwise
getEdges :: String -> [String]
getEdges s =
  if length s /= width * width then error "Non-square edges"
  else [topEdge s, rightEdge, bottomEdge s, leftEdge]
  where
    width = isqrt (length s)
    topEdge s1 = take width s1
    bottomEdge s1 = reverse $ drop (length s1 - width) s1
    rotS = rotateSquare s
    leftEdge = topEdge rotS
    rightEdge = bottomEdge rotS

placeNear 

placeTiles :: [Tile] -> M.Map (Complex Int) (Int, String)
placeTiles [] = mempty
placeTiles (Tile num grid:rest) = placer (M.singleton 0 (num, grid)) rest
  where
    placer :: M.Map (Complex Int) (Int, String) -> [Tile] -> M.Map (Complex Int) (Int, String)
    placer sofar rest =
      let allEdges :: M.Map String (Complex Int)
          allEdges = M.fromList $ concatMap (\(k, (_, v)) -> (,k) `map` getEdges v) $ M.toList sofar
          openRevEdges = [reverse edge | edge <- M.keys allEdges, reverse edge `M.notMember` allEdges]
          (placeables, 
      in undefined

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc20.in" else head args
  s <- readFile filename
  let tiles = parseTiles s
  mapM_ (\(Tile n td) -> print (n, getEdges td)) tiles
