{-# LANGUAGE ScopedTypeVariables #-}

import Control.Arrow (Arrow (first, second))
import Control.Monad.State.Strict
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Data.String
import System.Environment (getArgs)

-- import Debug.Trace (trace)

gshapes :: [[Int]]
gshapes =
    [ [15]
    , [2, 7, 2]
    , [4, 4, 7]
    , [1, 1, 1, 1]
    , [3, 3]
    ]

moveLeft :: [Int] -> [Int] -> ([Int], Bool)
moveLeft shape grid =
    let movedShape = map (`div` 2) shape
     in if any ((/= 0) . (.&. 1)) shape || any (/= 0) (zipWith (.&.) movedShape grid)
            then (shape, False)
            else (movedShape, True)

moveRight :: [Int] -> [Int] -> ([Int], Bool)
moveRight shape grid =
    let movedShape = map (* 2) shape
     in if any ((/= 0) . (.&. 64)) shape || any (/= 0) (zipWith (.&.) movedShape grid)
            then (shape, False)
            else (movedShape, True)

moveDown :: [Int] -> [Int] -> ([Int], Bool)
moveDown shape grid =
    let movedShape = 0 : shape
        merged = zipWith (.&.) movedShape grid
     in if (length merged /= length movedShape) || any (/= 0) merged
            then (shape, False)
            else (movedShape, True)

-- state is shapeIdx, jetIdx
placeShape :: [[Int]] -> B.ByteString -> [Int] -> State (Int, Int) [Int]
placeShape shapes jetData grid = do
    shape <- map (4 *) . (shapes !!) <$> gets ((`mod` length shapes) . fst)
    modify $ first succ
    let workGrid = map (const 0) shape ++ [0, 0, 0] ++ grid
    dropWhile (== 0) <$> dropGrid shape workGrid
  where
    dropGrid :: [Int] -> [Int] -> State (Int, Int) [Int]
    dropGrid shape workGrid = do
        jetidx <- gets snd
        let sidefunc = case jetData `BC.index` (jetidx `mod` B.length jetData) of
                '<' -> moveLeft
                '>' -> moveRight
                _ -> error "Bad jet movement"
        modify $ second succ
        let (shape', _) = sidefunc shape workGrid
        case moveDown shape' workGrid of
            (shape'', True) -> dropGrid shape'' workGrid
            (shape'', False) ->
                -- all done
                pure $ zipWith (.|.) (shape'' ++ repeat 0) workGrid

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc17.in"
                else head args
    s <- filter (not . isSpace) <$> readFile filename
    --let s = "<"
    let fst3 (a, _, _) = a
    let thd3 (_, _, c) = c
    let (finalGrid, stateRec) = flip evalState (0, 0) $ do
            let repeatM (0::Int) _ x = pure x
                repeatM n f x = f x >>= repeatM (n - 1) f
            repeatM
                2022
                ( \(g, m) -> do
                    g' <- placeShape gshapes (fromString s) g
                    m' <- (: m) . (\(a, b) -> (a, b, length g')) <$> get
                    pure (g', m')
                )
                ([], [])
    print $ length finalGrid
    -- hope we fell into a pattern by 2022
    let stateRecI = map (\(a, b, c) -> (toInteger a, toInteger b, toInteger c)) $ reverse stateRec
    let (lastPieceIdx, lastJetIdx, lastHeight) = last stateRecI
    let eqmod a b m = (a `mod` toInteger m) == (b `mod` toInteger m)
    let (prevPI, _, prevH) = head $ filter (\(a, b, _) -> eqmod a lastPieceIdx (length gshapes) && eqmod b lastJetIdx (length s)) $ init stateRecI
    let cycleLength = lastPieceIdx - prevPI
    let heightIncrease1 = ((1000000000000 - lastPieceIdx) `div` cycleLength) * (lastHeight - prevH)
    let cycleMimic = prevPI + ((1000000000000 - lastPieceIdx) `mod` cycleLength)
    let mimicHeight = thd3 . head $ filter ((==cycleMimic) . fst3) stateRecI
    let heightIncrease2 = mimicHeight - prevH

    print $ lastHeight + heightIncrease1 + heightIncrease2