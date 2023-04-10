-- stack --resolver lts-18.18 script --package multiset --package containers
{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall #-}

import Data.List

--import Debug.Trace
import Control.Applicative
import Control.Monad
import System.Environment

data SNum
  = Reg Int
  | Pair SNum SNum

instance Show SNum where
  show (Reg n) = show n
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

instance Read SNum where
  readsPrec _ s =
    [(Reg n, rst) | (n, rst) <- reads s] ++
    [ (Pair a b, rst)
    | ('[':s1) <- [s]
    , (a, s2) <- reads s1
    , (',':s3) <- [s2]
    , (b, s4) <- reads s3
    , (']':rst) <- [s4]
    ]

addToRightmost :: Int -> SNum -> SNum
addToRightmost n (Reg a) = Reg (a + n)
addToRightmost n (Pair a b) = Pair a (addToRightmost n b)

addToLeftmost :: Int -> SNum -> SNum
addToLeftmost n (Reg a) = Reg (a + n)
addToLeftmost n (Pair a b) = Pair (addToLeftmost n a) b

doExplode :: SNum -> Maybe SNum
doExplode s = (\(_, x, _) -> x) <$> go 0 s
  where
    go :: Int -> SNum -> Maybe (SNum -> SNum, SNum, SNum -> SNum)
    go _ (Reg _) = Nothing
    go n (Pair a b)
      | n < 4 =
        (\(lf, a', rf) -> (lf, Pair a' (rf b), id)) <$> go (n + 1) a <|>
        (\(lf, b', rf) -> (id, Pair (lf a) b', rf)) <$> go (n + 1) b
    go _ (Pair (Reg na) (Reg nb)) =
      Just (addToRightmost na, Reg 0, addToLeftmost nb)
    -- we shouldn't get to nesting level 4 and have anything other than
    -- a Pair with two Reg children
    go n s1 = error $ "Asked to explode " ++ show n ++ " and " ++ show s1

doSplit :: SNum -> Maybe SNum
doSplit (Reg n) =
  guard (n >= 10) >> pure (Pair (Reg $ n `div` 2) (Reg $ (n + 1) `div` 2))
doSplit (Pair a b) = ((`Pair` b) <$> doSplit a) <|> (Pair a <$> doSplit b)

normalize :: SNum -> SNum
normalize s = maybe s normalize (doExplode s <|> doSplit s)

addSNum :: SNum -> SNum -> SNum
addSNum a b = normalize $ Pair a b

magnitude :: SNum -> Int
magnitude (Reg n) = n
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

main :: IO ()
main = do
  args <- getArgs
  let filename = maybe "aoc18.in" fst $ uncons args
  datas <- words <$> readFile filename
  let snums = map read datas :: [SNum]
  print $ magnitude $ foldl' addSNum (head snums) (tail snums)
  print $
    maximum $
    [ magnitude (addSNum (snums !! i) (snums !! j))
    | i <- [0 .. length snums - 1]
    , j <- [0 .. length snums - 1]
    , i /= j
    ]
