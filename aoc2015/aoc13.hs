{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Data.Char
import Data.Functor (($>))
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import System.Environment
import Text.ParserCombinators.ReadP

parseInpLine :: String -> Maybe (String, String, Int)
-- "Bob would lose 61 happiness units by sitting next to Carol." -> ("Bob", "Carol", -61)
parseInpLine s = fst <$> listToMaybe (readP_to_S prsr s)
  where
    wordParser = munch1 isLetter
    gainLose = (string "gain" $> id) +++ (string "lose" $> negate)
    prsr :: ReadP (String, String, Int)
    prsr = do
      n1 <- wordParser
      _ <- string " would "
      xf <- gainLose
      pts <- readS_to_P reads
      _ <- string " happiness units by sitting next to "
      n2 <- wordParser
      string "." *> eof
      pure (n1, n2, xf pts)

evalSeating :: [(String, String, Int)] -> [String] -> Int
evalSeating rules guests =
  let sort2 a b = if a < b then (a, b) else (b, a)
      scoreMap = M.fromListWith (+) $ map (\(a, b, i) -> (sort2 a b, i)) rules
      pairs = zip guests (tail guests) ++ [(last guests, head guests)]
   in sum $ map (\(a, b) -> M.findWithDefault 0 (sort2 a b) scoreMap) pairs

maxHappiness :: [(String, String, Int)] -> Int
maxHappiness rules =
  let guests = M.fromList $ concatMap (\(a, b, _) -> [(a, ()), (b, ())]) rules
      guestList = map fst (M.toList guests)
      seatings = map (head guestList :) (Data.List.permutations (tail guestList))
   in maximum $ map (evalSeating rules) seatings

main :: IO ()
main = do
  args <- getArgs
  let filename = maybe "aoc13.in" fst $ uncons args
  datas <- lines <$> readFile filename
  let rules = mapM parseInpLine datas
  case rules of
    Nothing -> print "Error parsing rules"
    Just rules -> do
        print (maxHappiness rules)
        print (maxHappiness $ ("Me", "Me", 0):rules)