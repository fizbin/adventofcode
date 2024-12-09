{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import Control.Monad (forM_)
import Control.Monad.ST.Strict (ST)
import Data.Array (Array, (//))
import Data.Array qualified as A
import Data.Array.MArray qualified as MA
import Data.Array.ST qualified as STA
import Data.Char (isSpace)
import Data.Ix (Ix (rangeSize))
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Set qualified as S
import System.Environment (getArgs)

spec2dirmap :: [Int] -> Array Int (Maybe Int)
spec2dirmap spec = STA.runSTArray mkArr
  where
    annotate :: Bool -> Int -> Int -> [Int] -> [(Int, Int, Int)]
    annotate _ _ _ [] = []
    annotate True l f (s : ss) = (f, l, s) : annotate False (l + s) (f + 1) ss
    annotate False l f (s : ss) = annotate True (l + s) f ss
    annotatedSpec = annotate True 0 0 spec
    mkArr :: forall s. ST s (STA.STArray s _ _)
    mkArr = do
      dirmap <- MA.newArray (0, sum spec) Nothing
      forM_ annotatedSpec $ \(filenum, start, len) ->
        forM_ [0 .. len - 1] $ \off -> MA.writeArray dirmap (start + off) (Just filenum)
      pure dirmap

compactDirmap :: Array Int (Maybe Int) -> Array Int (Maybe Int)
compactDirmap dirMapIn = STA.runSTArray stAction
  where
    walkPtrs :: forall s. STA.STArray s _ _ -> Int -> _ -> ST s ()
    walkPtrs _ lft rgt | lft >= rgt = pure ()
    walkPtrs dirmap lft rgt = do
      lftval <- MA.readArray dirmap lft
      rgtval <- MA.readArray dirmap rgt
      case (lftval, rgtval) of
        (Nothing, Nothing) -> walkPtrs dirmap lft (rgt - 1)
        (Just _, Nothing) -> walkPtrs dirmap (lft + 1) (rgt - 1)
        (Just _, Just _) -> walkPtrs dirmap (lft + 1) rgt
        (Nothing, Just _) -> do
          MA.writeArray dirmap lft rgtval
          MA.writeArray dirmap rgt lftval
          walkPtrs dirmap (lft + 1) (rgt - 1)
    stAction :: forall s. ST s (STA.STArray s _ _)
    stAction = do
      dirmap <- MA.thaw dirMapIn
      uncurry (walkPtrs dirmap) (A.bounds dirMapIn)
      pure dirmap

getChecksum :: Array Int (Maybe Int) -> Int
getChecksum dirmap = sum $ mapMaybe (\(ix, val) -> (ix *) <$> val) (A.assocs dirmap)

spec2FreeAndFileMaps :: [Int] -> (S.Set (Int, Int), M.Map (Int, Int) Int)
spec2FreeAndFileMaps spec = (freeSet, fileMap)
  where
    annotate :: Bool -> Int -> Int -> [Int] -> [(Maybe Int, Int, Int)]
    annotate _ _ _ [] = []
    annotate True l f (s : ss) = (Just f, l, s) : annotate False (l + s) (f + 1) ss
    annotate False l f (s : ss) = (Nothing, l, s) : annotate True (l + s) f ss
    annotatedSpec = annotate True 0 0 spec
    freeSet =
      S.fromAscList $
        mapMaybe
          ( \(x, l, s) -> if isNothing x then Just (l, s) else Nothing
          )
          annotatedSpec
    fileMap = M.fromAscList $ mapMaybe (\(x, l, s) -> ((l, s),) <$> x) annotatedSpec

compactByFile :: (S.Set (Int, Int), M.Map (Int, Int) Int) -> (S.Set (Int, Int), M.Map (Int, Int) Int)
compactByFile (fs', fm) = go [] (freeSetBySize fs', fm)
  where
    freeSetBySize fs =
      M.map S.fromList (M.fromListWith (++) $ map (\(loc, sz) -> (sz, [(loc, sz)])) $ S.toList fs)
        `M.union` M.fromList (map (,S.empty) [0 .. 9])
    go outFile (freeSet, fileMap)
      | M.null fileMap = (S.unions freeSet, M.fromList outFile)
      | otherwise =
          let (((loc, sz), f), fileMap') = M.deleteFindMax fileMap
              minMaybe [] = Nothing
              minMaybe m = Just $ minimum m
              foundfreespot =
                minMaybe $
                  filter ((< loc) . fst) $
                    mapMaybe (\tsz -> S.lookupMin $ M.findWithDefault S.empty tsz freeSet) [sz .. 9]
           in case foundfreespot of
                Nothing -> go (((loc, sz), f) : outFile) (freeSet, fileMap')
                Just (freeloc, freesz) ->
                  let restFree = M.adjust (S.delete (freeloc, freesz)) freesz freeSet
                      newFree = M.adjust (S.insert (freeloc + sz, remaining)) remaining restFree
                      remaining = freesz - sz
                      outFile' = ((freeloc, sz), f) : outFile
                   in if remaining == 0
                        then go outFile' (restFree, fileMap')
                        else go outFile' (newFree, fileMap')

freeFilePair2dirmap :: (S.Set (Int, Int), M.Map (Int, Int) Int) -> Array Int (Maybe Int)
freeFilePair2dirmap (freeSet, fileMap) =
  let maxLocSz = max (fromMaybe (0, 0) (S.lookupMax freeSet)) (maybe (0, 0) fst (M.lookupMax fileMap))
      diskBounds = (0, uncurry (+) maxLocSz)
      startArray = A.listArray diskBounds (replicate (rangeSize diskBounds) Nothing)
      fileAssocs = concatMap (\((loc, sz), f) -> map (,Just f) [loc .. loc + sz - 1]) $ M.toList fileMap
   in startArray // fileAssocs

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc9.in" else head args
  specStr <- takeWhile (not . isSpace) <$> readFile filename
  let spec = map (read . (: [])) specStr
  let part1DirMap = compactDirmap $ spec2dirmap spec
  putStrLn $ "Part 1: " ++ show (getChecksum part1DirMap)
  let part2DirMap = freeFilePair2dirmap $ compactByFile $ spec2FreeAndFileMaps spec
  putStrLn $ "Part 2: " ++ show (getChecksum part2DirMap)
