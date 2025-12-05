import Data.List (foldl', sort)
import System.Environment (getArgs)

newtype IdRange =
  IdRange (Integer, Integer)
  deriving (Eq, Ord, Show)

instance Read IdRange where
  readsPrec _ s =
    [ (IdRange (low, hi), s''')
    | (low, s') <- reads s
    , ('-':s'') <- [s']
    , (hi, s''') <- reads s''
    ]

inRange :: Integer -> IdRange -> Bool
inRange idVal (IdRange (lo, hi)) = (idVal >= lo) && (idVal <= hi)

rangeSize :: IdRange -> Integer
rangeSize (IdRange (lo, hi)) = hi - lo + 1

mergeRange :: IdRange -> IdRange -> (Maybe IdRange, IdRange)
mergeRange r1 r2 =
  let IdRange (lo1, hi1) = min r1 r2
      IdRange (lo2, hi2) = max r1 r2
   in if lo2 <= hi1 + 1
        then (Nothing, IdRange (lo1, max hi1 hi2))
        else (Just (IdRange (lo1, hi1)), IdRange (lo2, hi2))

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc5.in"
          (x:_) -> x
  dataLines <- lines <$> readFile filename
  let (rangeStrs, idStrs) = break null dataLines
  let ranges = map read rangeStrs :: [IdRange]
  let myIds = map read $ words (unwords idStrs)
  print $ length $ filter (\ival -> any (inRange ival) ranges) myIds
  print
    $ case sort ranges of
        [] -> 0
        (r:rs) ->
          let (finalsize, finalrange) =
                foldl'
                  (\(sz, rg1) rg2 ->
                     case mergeRange rg1 rg2 of
                       (Nothing, x) -> (sz, x)
                       (Just r', x) -> (sz + rangeSize r', x))
                  (0, r)
                  rs
           in finalsize + rangeSize finalrange
