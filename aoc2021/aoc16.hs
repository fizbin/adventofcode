-- stack --resolver lts-18.18 script --package multiset --package containers
{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall #-}


import Data.Bits
import Data.Char (ord)
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

readBinV :: String -> Int
readBinV s =
  let (n, "") = head $ readInt 2 (`elem` "01") (\c -> ord c - 48) s
   in n

readHexV :: [Char] -> Int
readHexV s =
  let (n, "") = head $ readHex s
   in n

parseBint :: String -> (Int, String)
parseBint = go 0
  where
    go acc ('0':rest) = (16 * acc + readBinV (take 4 rest), drop 4 rest)
    go acc ('1':rest) = go (16 * acc + readBinV (take 4 rest)) (drop 4 rest)
    go _ r = error $ "bad bigit " ++ show r

parseVersion :: String -> (Int, String)
parseVersion indat =
  let verbin = take 3 indat
      tidbin = take 3 $ drop 3 indat
      ver = readBinV verbin
      ndata =
        if tidbin == "100"
          then snd (parseBint $ drop 6 indat)
          else mydrop (drop 6 indat)
   in (ver, ndata)
  where
    mydrop ('1':rest) = drop 11 rest
    mydrop ('0':rest) = drop 15 rest
    mydrop r = error $ "Bad bigit " ++ show r

allVersions :: String -> [Int]
allVersions "" = []
allVersions s
  | all (== '0') s = []
allVersions rest =
  let (n, rest') = parseVersion rest
   in n : allVersions rest'

tidToFunc :: Int -> ([Int] -> Int)
tidToFunc 0 = sum
tidToFunc 1 = product
tidToFunc 2 = minimum
tidToFunc 3 = maximum
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

parseValue :: String -> (Int, String)
parseValue indat =
  let tidbin = take 3 $ drop 3 indat
      tid = readBinV tidbin
      (subvals, subrest) = mysub $ drop 6 indat
   in if tidbin == "100"
        then parseBint $ drop 6 indat
        else (tidToFunc tid subvals, subrest)
  where
    mysub ('1':rest) =
      let npackets = readBinV (take 11 rest)
          parselist = iterate (parseValue . snd) (0, drop 11 rest)
       in ( map fst (take npackets (tail parselist))
          , snd (parselist !! npackets))
    mysub ('0':rest) =
      let nbits = readBinV (take 15 rest)
          subdata = take nbits (drop 15 rest)
          rest' = drop (15 + nbits) rest
          vals = parseAll subdata
       in (vals, rest')
    mysub r = error $ "Can't decompose " ++ show (take 20 r)
    parseAll "" = []
    parseAll s =
      let (v, s') = parseValue s
       in v : parseAll s'

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc16.in"
          else head args
  datas <- head . words <$> readFile filename
  let bindata = concatMap (tail . showBin . (.|. 16) . readHexV . (: [])) datas
  print $ sum $ allVersions bindata
  print $ parseValue bindata
