{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

import Control.Arrow (Arrow (second))
import Control.Monad (foldM)
import Data.Bits (Bits ((.&.), (.|.)), (.^.))
import Data.Char (isSpace)
import Data.Either (isRight)
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, listToMaybe)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, munch, munch1, readP_to_S, readS_to_P, string)

data WireOp = AND | OR | XOR deriving (Read, Show, Eq)

doWire :: WireOp -> Int -> Int -> Int
doWire AND x y = x .&. y
doWire OR x y = x .|. y
doWire XOR x y = x .^. y

parseComputeLine :: String -> (String, WireOp, String, String)
parseComputeLine s = case filter (null . snd) $ readP_to_S parser s of
  [] -> error $ "No valid parse for " ++ show s
  (ans, _) : _ -> ans
  where
    parser :: ReadP (String, WireOp, String, String)
    parser =
      (,,,)
        <$> munch1 (not . isSpace)
        <*> readS_to_P reads
        <*> (munch isSpace *> munch1 (not . isSpace))
        <*> (munch isSpace *> string "->" *> munch isSpace *> munch (not . isSpace))
        <* munch isSpace

doCompute :: M.Map String Int -> M.Map String (WireOp, String, String) -> Maybe Int
doCompute initMap computeMap =
  let notifyMap =
        M.fromListWith (++) $
          concatMap (\(d, (_, a, c)) -> [(a, [d]), (c, [d])]) $
            M.toList computeMap
      go [] valMap = valMap
      go (q : queue) valMap = case M.lookup q computeMap of
        Nothing -> go (queue ++ M.findWithDefault [] q notifyMap) valMap
        Just (op, lft, rgt) ->
          case (`M.lookup` valMap) <$> [q, lft, rgt] of
            [Nothing, Just ell, Just arr] ->
              go
                (M.findWithDefault [] q notifyMap ++ queue)
                (M.insert q (doWire op ell arr) valMap)
            _ -> go queue valMap
      finalVals = go (M.keys initMap) initMap
      zKeys = sort $ filter ((== 'z') . head) (M.keys computeMap)
   in foldM (\b a -> (2 * b +) <$> M.lookup a finalVals) 0 (reverse zKeys)

fixAddition :: M.Map String (WireOp, String, String) -> [(String, String)]
fixAddition computeMap =
  let allInputs = sort $ nub $ concatMap (\(_, a, b) -> [a, b]) (M.elems computeMap)
      xKeys = filter ((== 'x') . head) allInputs
      yKeys = filter ((== 'y') . head) allInputs
      zKeys = sort $ filter ((== 'z') . head) (M.keys computeMap)
      tryAddition cMap a b =
        let xvals = snd $ mapAccumL (\x w -> (x `div` 2, (w, x `mod` 2))) a xKeys
            yvals = snd $ mapAccumL (\y w -> (y `div` 2, (w, y `mod` 2))) b yKeys
            initMap = M.fromList $ xvals ++ yvals
         in case doCompute initMap cMap of
              Nothing -> Left zKeys
              Just x ->
                if x == a + b
                  then Right ()
                  else
                    let wrongbits = x .^. (a + b)
                        go _ [] = []
                        go n (w : ws)
                          | n `mod` 2 == 1 = w : go (n `div` 2) ws
                          | otherwise = go (n `div` 2) ws
                     in Left (go wrongbits zKeys)
      badSpot =
        listToMaybe
          [ (xw, yw, wrongOuts, testVals)
            | (xw, yw) <- zip xKeys yKeys ++ zip (tail xKeys) yKeys,
              let xexp = read (tail xw) :: Int,
              let yexp = read (tail yw) :: Int,
              let testVals =
                    [ (k1 * (2 ^ max 0 (xexp - 1)), k2 * (2 ^ max 0 (yexp - 1)))
                      | k1 <- [0 .. 9],
                        k2 <- [0 .. 9]
                    ],
              Left wrongOuts <- [tryAddition computeMap (2 ^ xexp) (2 ^ yexp)]
          ]
      upstream :: Int -> String -> [String]
      upstream 0 w = [w]
      upstream lvl w = case M.lookup w computeMap of
        Nothing -> []
        Just (_, a, b) -> concatMap (upstream (lvl - 1)) [a, b]
      computeSwap w1 w2 =
        M.alter (const $ M.lookup w2 computeMap) w1 $
          M.alter (const $ M.lookup w1 computeMap) w2 computeMap
      fixSpot = case badSpot of
        Nothing -> Nothing
        Just (xw, yw, outWrongs, testVals) ->
          let testWires = sort . nub $ do lvl <- [0 .. 4]; outWrong <- outWrongs; upstream lvl outWrong
              combos = [(ow1, ow2) | (ow1 : rst) <- tails testWires, ow2 <- rst]
              goodSwaps =
                [ (ow1, ow2)
                  | (ow1, ow2) <- combos,
                    M.member ow1 computeMap,
                    M.member ow2 computeMap,
                    let cMap = computeSwap ow1 ow2,
                    all (isRight . uncurry (tryAddition cMap)) testVals
                ]
           in case goodSwaps of
                [swp] -> Just swp
                [] -> error $ "Can't figure out how to fix spot " ++ show (xw, yw)
                lst -> error $ "Ambiguous solution for " ++ show (xw, yw) ++ ": " ++ show lst
   in case fixSpot of
        Nothing -> []
        Just (w1, w2) -> (w1, w2) : fixAddition (computeSwap w1 w2)

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc24.in" else head args
  (init_spec, gate_spec) <-
    splitOn "\n\n" <$> readFile filename >>= \case
      [a, b] -> pure (a, b)
      paras -> ioError (userError $ "Wrong number of paragraphs; expected 2, got " ++ show (length paras))
  let initMap = M.fromList $ second (read . tail) . break (== ':') <$> lines init_spec
  let computeParsed = parseComputeLine <$> lines gate_spec
      computeMap = M.fromList $ (\(a, b, c, d) -> (d, (b, a, c))) <$> computeParsed

  -- print computeMap
  -- print initMap
  putStr "Part 1: "
  print $ fromJust $ doCompute initMap computeMap
  let swaps = fixAddition computeMap
      swapbits = sort $ concatMap (\(a, b) -> [a, b]) swaps
  putStrLn $ "Part 2: " ++ intercalate "," swapbits
