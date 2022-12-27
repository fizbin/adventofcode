import Data.Char (isDigit)
import Data.List (elemIndex, foldl', sort)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

-- import Debug.Trace

numbers :: String -> [Int]
numbers [] = []
numbers s = case reads s of
    [] -> numbers $ dropWhile (\ch -> ch /= '-' && not (isDigit ch)) (tail s)
    (n, rest) : _ -> n : numbers rest

mix :: [Int] -> [Int] -> [Int]
mix datas orderList =
    foldl' mix1 orderList (zip [0 ..] datas)
  where
    ldata = length datas
    mix1 ordering (idx, shiftval) =
        let rval = shiftval `mod` (ldata - 1)
            lval = (ldata - 1) - rval
         in -- trace (show $ (datas !!) <$> ordering) $
            walky idx lval rval ordering []
    walky target _ _ [] _ = error $ "Walked off the end looking for " ++ show target
    walky target lval rval (o : os) rev
        | target == o =
            if length os >= rval
                then let (x, y) = splitAt rval os in reverse rev ++ x ++ (o : y)
                else let (x, y) = splitAt lval rev in reverse (x ++ (o : y)) ++ os
        | otherwise = walky target lval rval os (o : rev)

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
    -- let nums = [1, 2, -3, 3, -2, 0, 4]
    --    print nums
    let mixed = mix nums [0 .. length nums - 1]
    let mixedinv = (snd <$>) . sort $ zip mixed [0 ..]
    let zeroIndex = fromJust (elemIndex 0 nums)
    let zeroSpot = mixedinv !! zeroIndex
    print $ sum $ (nums !!) . (mixed !!) . (`mod` ldata) . (zeroSpot +) <$> [1000, 2000, 3000]
    let applyN 0 _ x = x; applyN n f x = applyN (n - (1 :: Int)) f (f x)
    let mixed2 = applyN 10 (mix $ (811589153 *) <$> nums) [0 .. length nums - 1]
    let mixed2inv = (snd <$>) . sort $ zip mixed2 [0 ..]
    let zeroSpot2 = mixed2inv !! zeroIndex
    print $ (811589153 *) $ sum $ (nums !!) . (mixed2 !!) . (`mod` ldata) . (zeroSpot2 +) <$> [1000, 2000, 3000]
