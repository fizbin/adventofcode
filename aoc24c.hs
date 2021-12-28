-- stack --resolver lts-18.18 script
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad
import Data.Char
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Semigroup
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

-- This is what those 18 instructions compute. Output is new z value
runCommand :: Command -> Int -> Int -> Int
runCommand Command {..} inp zIn =
  let inpTarget = (zIn `mod` 26) + xadd
      zNew = zIn `div` zdiv
   in if inp == inpTarget
        then zNew
        else 26 * zNew + yadd + inp

-- max allowable Z value at start of this series of commands, if we're to reach z=0 when done
-- err on the side of "too large", so that we cut off fewer possibilities
allowableZ :: Command -> Int -> Int
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
allowableZ Command {..} maxOutZ =
  max ((maxOutZ + 1) * zdiv - 1) $
  maximum [(((maxOutZ - yadd - inp) `div` 26) + 1) * zdiv - 1 | inp <- [1 .. 9]]

findAcceptable :: (Applicative f, Semigroup (f String)) => [Command] -> f String
findAcceptable cmd0 = (IM.!) (go (IM.singleton 0 (pure "")) cmdWithMaxZOut) 0
  where
    cmdWithMaxZOut :: [(Command, Int)]
    cmdWithMaxZOut = zip cmd0 (tail $ scanr allowableZ 0 cmd0)
    go m [] = m
    go m ((cmd, maxPostZ):cmds) =
      let a =
            map
              (\inp ->
                 IM.mapKeysWith
                   (<>)
                   (runCommand cmd inp)
                   (IM.map (fmap (++ show inp)) m))
              [1 .. 9]
          b = map (IM.filterWithKey (\k _ -> k <= maxPostZ)) a
       in go (IM.unionsWith (<>) b) cmds

newtype Both f g a =
  Both
    { unBoth :: (f a, g a)
    }
  deriving (Show, Eq, Ord)

instance (Functor f, Functor g) => Functor (Both f g) where
  fmap f (Both (a, b)) = Both (f <$> a, f <$> b)

instance (Applicative f, Applicative g) => Applicative (Both f g) where
  (Both (a, b)) <*> (Both (c, d)) = Both (a <*> c, b <*> d)
  pure x = Both (pure x, pure x)

instance (Semigroup (f a), Semigroup (g a)) => Semigroup (Both f g a) where
  (Both (a, b)) <> (Both (c, d)) = Both (a <> c, b <> d)

main :: IO ()
main = do
  args <- getArgs
  let filename = maybe "aoc24.in" fst $ uncons args
  datas <- head . readCommands <$> readFile filename
  unless (null $ snd datas) (ioError $ userError "Bad parse")
  let commands = fst datas
  let res = findAcceptable commands
  putStrLn $ getMax $ fst $ unBoth res
  putStrLn $ getMin $ snd $ unBoth res
