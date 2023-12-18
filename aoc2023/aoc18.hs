{-# LANGUAGE BangPatterns #-}

import Data.Char (digitToInt)
import System.Environment (getArgs)

data Direction = LDir | RDir | UDir | DDir deriving (Eq)

area :: [(Direction, Int)] -> Int
area instrs =
    let (perilen, twoarea) = go (0, 0) 0 0 instrs
     in (perilen + abs twoarea + 2) `div` 2
  where
    go _ peri ta [] = (peri, ta)
    go !coords !peri !ta ((digdir, diglen) : rest) =
        let newcoords = case digdir of
                LDir -> (fst coords, snd coords - diglen)
                RDir -> (fst coords, snd coords + diglen)
                UDir -> (fst coords - diglen, snd coords)
                DDir -> (fst coords + diglen, snd coords)
         in go newcoords (peri + diglen) (ta + fst coords * snd newcoords - fst newcoords * snd coords) rest

parse1 :: String -> (Direction, Int)
parse1 line = case words line of
    ("U" : (len : _)) -> (UDir, read len)
    ("D" : (len : _)) -> (DDir, read len)
    ("L" : (len : _)) -> (LDir, read len)
    ("R" : (len : _)) -> (RDir, read len)
    _ -> error $ "parse1: " ++ show line

parse2 :: String -> (Direction, Int)
parse2 line =
    let thirdword = words line !! 2
     in case thirdword !! 7 of
            '0' -> (RDir, fromHex $ take 5 (drop 2 thirdword))
            '1' -> (DDir, fromHex $ take 5 (drop 2 thirdword))
            '2' -> (LDir, fromHex $ take 5 (drop 2 thirdword))
            '3' -> (UDir, fromHex $ take 5 (drop 2 thirdword))
            _ -> error $ "parse2: " ++ show thirdword
  where
    fromHex = foldl (\x -> ((16 * x) +) . digitToInt) 0

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc18.in" else head args
    s <- lines <$> readFile filename
    print $ area (map parse1 s)
    print $ area (map parse2 s)
