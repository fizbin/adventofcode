-- stack --resolver lts-18.18 script --package multiset --package containers

{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Arrow
import Data.Bits
import Data.Char (ord)
import Data.Maybe (fromJust, isJust, listToMaybe)
--import Debug.Trace
import Numeric (readHex, readInt, showIntAtBase)
import System.Environment

bigitToChar :: Int -> Char
bigitToChar 0 = '0'
bigitToChar 1 = '1'
bigitToChar _ = error "bad bigit"

showsBin :: Int -> ShowS
showsBin = showIntAtBase 2 bigitToChar

showBin :: Int -> String
showBin n = showsBin n ""

readBin :: ReadS Int
readBin = readInt 2 (`elem` "01") (\c -> ord c - 48)

readHexV :: [Char] -> Int
readHexV s =
  let (n, "") = head $ readHex s
   in n

readBint :: ReadS Int
readBint = go 0
  where
    go acc s =
      [ (16 * acc + val, drop 4 rest)
        | ('0' : rest) <- [s],
          (val, "") <- readBin (take 4 rest)
      ]
        ++ [ x
             | ('1' : rest) <- [s],
               (val, "") <- readBin (take 4 rest),
               x <- go (16 * acc + val) (drop 4 rest)
           ]

readVersion :: String -> [(Int, String)]
readVersion s =
  [ (ver, final)
    | (verbin, rest1) <- [splitAt 3 s],
      (ver, "") <- readBin verbin,
      (tidbin, rest2) <- [splitAt 3 rest1],
      (tid, "") <- readBin tidbin,
      final <- skipPacket tid rest2
  ]
  where
    skipPacket _ [] = []
    skipPacket 4 s' = map snd $ readBint s'
    skipPacket _ ('1' : rest) = [drop 11 rest]
    skipPacket _ ('0' : rest) = [drop 15 rest | length rest >= 15]
    skipPacket _ other = error $ "Bad data: " ++ show (take 20 other)

allVersions :: String -> [([Int], String)]
allVersions s =
  [ (v1 : vals, rest)
    | (v1, s') <- readVersion s,
      (vals, rest) <- allVersions s'
  ]
    ++ [([], s)]

tidToFunc :: Int -> ([Int] -> Int)
tidToFunc 0 = sum
tidToFunc 1 = product
tidToFunc 2 = minimum
tidToFunc 3 = maximum
tidToFunc 4 = head
tidToFunc 5 =
  \[a, b] ->
    if a > b
      then 1
      else 0
tidToFunc 6 =
  \[a, b] ->
    if a < b
      then 1
      else 0
tidToFunc 7 =
  \[a, b] ->
    if a == b
      then 1
      else 0
tidToFunc n = error $ "Unknown func " ++ show n

readValue :: String -> [(Int, String)]
readValue s =
  [ (tidToFunc tid values, final)
    | (_, rest1) <- [splitAt 3 s],
      (tidbin, rest2) <- [splitAt 3 rest1],
      (tid, "") <- readBin tidbin,
      (decider : rest3) <- [rest2],
      (values, final) <- packetInternals tid decider rest3
  ]
  where
    packetInternals :: Int -> Char -> String -> [([Int], String)]
    packetInternals 4 d r = map (first (: [])) $ readBint (d : r)
    packetInternals _ '1' r = do
      let (npacketbin, rest) = splitAt 11 r
      (npacket, "") <- readBin npacketbin
      let parseForever = iterate (head . readValue . snd) (0, rest)
      pure
        ( map fst (take npacket $ tail parseForever),
          snd (parseForever !! npacket)
        )
    packetInternals _ '0' r = do
      let (nbitsbin, rest) = splitAt 15 r
      (nbits, "") <- readBin nbitsbin
      let (subpart, rest') = splitAt nbits rest
      let parseForever =
            iterate (>>= (listToMaybe . readValue . snd)) (Just (0, subpart))
      let parsed = takeWhile isJust parseForever
      pure (map (fst . fromJust) $ tail parsed, rest')
    packetInternals _ _ r = error $ "blarg" ++ show r

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc16.in"
          else head args
  datas <- head . words <$> readFile filename
  let bindata = concatMap (tail . showBin . (.|. 16) . readHexV . (: [])) datas
  print $
    map (first sum) $ takeWhile ((< 50) . length . snd) $ allVersions bindata
  print $ readValue bindata
