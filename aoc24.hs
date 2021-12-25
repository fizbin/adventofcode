-- stack --resolver lts-18.18 script
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad
import Data.Char
import Data.List
import System.Environment

-- Don't build an engine that can parse and deal with arbitrary instructions.
-- Instead, deal only with the input we have.
-- What we have is a series of similar instruction sequences, 18 instructions long
-- typical 18-instruction sequence:
-- inp w
-- mul x 0
-- add x z
-- mod x 26
-- div z 26 -- The "26" here varies: can be 26 or 1
-- add x -8 -- The second number here varies
-- eql x w
-- eql x 0
-- mul y 0
-- add y 25
-- mul y x
-- add y 1
-- mul z y
-- mul y 0
-- add y w
-- add y 3  -- The second number here varies
-- mul y x
-- add z y
-- Note that an 18-instruction sequence depends only on the three numbers that vary.
-- Also, note that the only important state from one 18-instruction sequence to the
-- next is the "z" variable - other variables are wiped out when the next sequence
-- starts.
-- Represents our 18-instruction sequence from "inp w" to "add z y"
data Command =
  Command
    { xadd :: Int
    , yadd :: Int
    , zdiv :: Int
    }
  deriving (Show, Eq)

readCommand :: String -> [(Command, String)]
readCommand s0 = do
  let require pref s = [((), drop (length pref) s) | pref `isPrefixOf` s]
  (_, s1) <-
    require
      "inp w\n\
      \mul x 0\n\
      \add x z\n\
      \mod x 26\n\
      \div z "
      s0
  (zdiv, s2) <- reads s1
  (_, s3) <- require "\nadd x " s2
  (xadd, s4) <- reads s3
  (_, s5) <-
    require
      "\neql x w\n\
       \eql x 0\n\
       \mul y 0\n\
       \add y 25\n\
       \mul y x\n\
       \add y 1\n\
       \mul z y\n\
       \mul y 0\n\
       \add y w\n\
       \add y "
      s4
  (yadd, s6) <- reads s5
  (_, s7) <- require "\nmul y x\nadd z y" s6
  pure (Command {..}, s7)

readCommands :: String -> [([Command], String)]
readCommands "" = [([], "")]
readCommands (s:ss)
  | isSpace s = readCommands ss
readCommands s = do
  (c, rest) <- readCommand s
  (restc, finals) <- readCommands rest
  pure (c : restc, finals)

-- This is what those 18 instructions compute. Outpus is new z value
runCommand :: Command -> Int -> Int -> Int
runCommand Command {..} inp zIn =
  let inpTarget = (zIn `mod` 26) + xadd
      zNew = zIn `div` zdiv
   in if inp == inpTarget
        then zNew
        else 26 * zNew + yadd + inp

-- max allowable Z value at start of this series of commands, if we're to reach z=0 when done
-- err on the side of "too large", so that we cut off fewer possibilities
allowableZ :: [Command] -> Int
allowableZ [] = 0
-- if we have an input "z" value of zIn, then our output z value is one of:
-- - zIn `div` zdiv
-- - 26*(zIn `div` zdiv) + yadd + inp
-- We must determine the maximum zIn so that that output z value is <= allowableZ rest
-- remember inp can be from 1 to 9
-- First case:
--   zIn `div` zdiv <= maxOutZ
--   zIn <= (maxOutZ + 1) * zdiv - 1
-- Second case:
--   26*(zIn `div` zdiv) + yadd + inp <= maxOutZ
--   26*(zIn `div` zdiv) <= maxOutZ - yadd - inp
--   (zIn `div` zdiv) <= (maxOutZ - yadd - inp) `div` 26
--   zin <= (((maxOutZ - yadd - inp) `div` 26) + 1) * zdiv - 1
allowableZ (Command {..}:rest) =
  let maxOutZ = allowableZ rest
   in max ((maxOutZ + 1) * zdiv - 1) $
      maximum
        [(((maxOutZ - yadd - inp) `div` 26) + 1) * zdiv - 1 | inp <- [1 .. 9]]

findAcceptable :: [Int] -> [Command] -> [[Int]]
findAcceptable inputs cmd0 = go 0 cmdWithMaxZOut
  where
    cmdWithMaxZOut :: [(Command, Int)]
    cmdWithMaxZOut = zipWith (\c cs -> (c, allowableZ cs)) cmd0 (tail $ tails cmd0)
    go 0 [] = [[]]
    go _ [] = []
    go pz ((cmd, maxPostZ):cmds) = do
      inp <- inputs
      let nextZ = runCommand cmd inp pz
      guard $ nextZ <= maxPostZ
      (inp :) <$> go nextZ cmds

main :: IO ()
main = do
  args <- getArgs
  let filename = maybe "aoc24.in" fst $ uncons args
  datas <- head . readCommands <$> readFile filename
  unless (null $ snd datas) (ioError $ userError "Bad parse")
  let commands = fst datas
  putStrLn $ concatMap show $ head $ findAcceptable (reverse [1 .. 9]) commands
  putStrLn $ concatMap show $ head $ findAcceptable [1 .. 9] commands
