{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.List (scanl')
import System.Environment (getArgs)

newtype Snafu = Snafu Int deriving (Eq, Ord, Num)

instance Read Snafu where
    readsPrec _ s = do
        (r1, s') <- read1 s
        go r1 s'
      where
        read1 ('2' : s) = [(2, s)]
        read1 ('1' : s) = [(1, s)]
        read1 ('0' : s) = [(0, s)]
        read1 ('-' : s) = [(-1, s)]
        read1 ('=' : s) = [(-2, s)]
        read1 s = []
        go accum s =
            let nxt = read1 s
             in if null nxt
                    then [(Snafu accum, s)]
                    else go (5 * accum + fst (head nxt)) (snd $ head nxt)

instance Show Snafu where
  showsPrec _ (Snafu 0) = ('0':)
  showsPrec _ (Snafu x) = let (off, sym) = case x `mod` 5 of
                                    0 -> (0, '0')
                                    1 -> (1, '1')
                                    2 -> (2, '2')
                                    3 -> (-2, '=')
                                    4 -> (-1, '-')
                            in if off == x then (sym:)
                               else shows (Snafu ((x - off) `div` 5)) . (sym:)

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc25.in"
                else head args
    s <- (read <$>) . lines <$> readFile filename :: IO [Snafu]
    print $ sum s