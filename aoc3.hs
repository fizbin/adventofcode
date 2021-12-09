import System.Environment
import Data.List
import Numeric 
import Data.Char

middle :: [Char] -> Char
middle [] = '1'
middle [a] = a
middle [a, b] = b
middle (x:xs) = middle (init xs)

mostCommon :: [Char] -> Char
mostCommon xs = middle $ sort xs

readBin :: String -> Int
readBin s = val1
  where
    [(val1, "")] = readInt 2 (`elem` "01") digitToInt s

p1 :: [String] -> Int
p1 ilist = val1 * val2
  where
    val1 = readBin (map mostCommon $ transpose ilist)
    val2 = readBin (map (opp . mostCommon) $ transpose ilist)
    opp '0' = '1'
    opp _ = '0'

winnow :: (Char -> Char) -> [String] -> Int -> String
winnow _ [] _ = error "Empty list winnowing"
winnow _ [x] _ = x
winnow b xs n = let
  digs = map (!! n) xs
  mostC = mostCommon digs
  filterp str = str !! n == b mostC
  winnowed = [x | x <- xs, filterp x]
  in winnow b winnowed (n+1)

p2 :: [String] -> Int
p2 ilist = findThing id * findThing opp
  where
    findThing b = readBin $ winnow b ilist 0
    opp '0' = '1'
    opp _ = '0'

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc3.in" else head args
  strs <- words <$> readFile filename
  print $ p1 strs
  print $ p2 strs
  