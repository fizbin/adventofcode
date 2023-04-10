import Control.Monad.ST
import Data.Char (isDigit)
import Data.Foldable
import Data.List (elemIndex, foldl', sort)
import Data.Maybe (fromJust)
import Data.Vector.Generic (basicUnsafeFreeze, basicUnsafeThaw)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Environment (getArgs)

import Debug.Trace

numbers :: String -> [Int]
numbers [] = []
numbers s = case reads s of
    [] -> numbers $ dropWhile (\ch -> ch /= '-' && not (isDigit ch)) (tail s)
    (n, rest) : _ -> n : numbers rest

mix :: [Int] -> U.Vector Int -> U.Vector Int
mix datas orderList = runST $
    do
        orderList' <- basicUnsafeThaw orderList
        mapM_ (mix1 orderList') (zip [0 ..] datas)
        basicUnsafeFreeze orderList'
  where
    ldata = U.length orderList
    mix1 :: UM.MVector s Int -> (Int, Int) -> ST s ()
    mix1 ordering (idx, shiftval) =
        let rval = shiftval `mod` (ldata - 1)
            lval = (ldata - 1) - rval
         in do
                jtidx <- UM.ifoldrM (\i a res -> pure $ if a == idx then Just i else res) Nothing ordering
                tIdx <- case jtidx of
                    Just x -> pure x
                    Nothing -> basicUnsafeFreeze ordering >>= \v -> error ("Nope; looking for " ++ show idx ++ " in " ++ show (U.toList v))
                if tIdx >= lval
                    then do
                        UM.move (UM.slice (tIdx - lval + 1) lval ordering) (UM.slice (tIdx - lval) lval ordering)
                        UM.write ordering (tIdx - lval) idx
                    else do
                        UM.move (UM.slice tIdx rval ordering) (UM.slice (tIdx + 1) rval ordering)
                        UM.write ordering (tIdx + rval) idx

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc20.in"
                else head args
    s <- readFile filename
    let nums = numbers s
    let ldata = length nums
    let mixed = mix nums (U.generate (length nums) id)
    let zeroIndex = fromJust (elemIndex 0 nums)
    let zeroSpot = fromJust $ U.elemIndex zeroIndex mixed
    print $ sum $ (nums !!) . (mixed U.!) . (`mod` ldata) . (zeroSpot +) <$> [1000, 2000, 3000]
    let applyN 0 _ x = x; applyN n f x = applyN (n - (1 :: Int)) f (f x)
    let mixed2 = applyN 10 (mix $ (811589153 *) <$> nums) (U.generate (length nums) id)
    let zeroSpot2 = fromJust $ U.elemIndex zeroIndex mixed2
    print $ (811589153 *) $ sum $ (nums !!) . (mixed2 U.!) . (`mod` ldata) . (zeroSpot2 +) <$> [1000, 2000, 3000]
