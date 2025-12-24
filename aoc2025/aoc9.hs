import Data.List (tails)
import System.Environment (getArgs)

part1 :: [(Int, Int)] -> Int
part1 pts =
  maximum
    [ (max a c - min a c + 1) * (max b d - min b d + 1)
    | ((a, b):pts') <- tails pts
    , (c, d) <- pts'
    ]

part2 :: [(Int, Int)] -> Int
part2 [] = 0
part2 pts@(ptsh:ptst) =
  maximum [area p1 p2 | (p1:pts') <- tails pts, p2 <- pts', acceptable p1 p2]
  where
    area (a, b) (c, d) = (max a c - min a c + 1) * (max b d - min b d + 1)
    segments = zip pts (ptst ++ [ptsh])
    strictlyBetween a b c = (a < b) && (b < c)
    rangeIntersects a b c d = strictlyBetween a c b || strictlyBetween c b d
    invalidates tl br ((p0x, p0y), (p1x, p1y)) =
      (strictlyBetween (fst tl) p0x (fst br)
         && strictlyBetween (snd tl) p0y (snd br))
        || (strictlyBetween (fst tl) p1x (fst br)
              && strictlyBetween (snd tl) p1y (snd br))
        || ((p1y < p0y)
              && (p0x > fst tl)
              && (p0x <= fst br)
              && rangeIntersects p1y p0y (snd tl) (snd br) -- left
            )
        || ((p1y > p0y)
              && (p0x >= fst tl)
              && (p0x < fst br)
              && rangeIntersects p0y p1y (snd tl) (snd br) -- right
            )
        || ((p1x < p0x)
              && (p0y >= snd tl)
              && (p0y < snd br)
              && rangeIntersects p1x p0x (fst tl) (fst br) -- up
            )
        || ((p1x > p0x)
              && (p0y > snd tl)
              && (p0y <= snd br)
              && rangeIntersects p0x p1x (fst tl) (fst br) -- down
            )
    acceptable :: (Int, Int) -> (Int, Int) -> Bool
    acceptable (a, b) (c, d) =
      let tl = (min a c, min b d)
          br = (max a c, max b d)
       in not (any (invalidates tl br) segments)

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc9.in"
          (x:_) -> x
  dataLines <- lines <$> readFile filename
  let dataPoints = map (\x -> read $ "(" ++ x ++ ")") dataLines :: [(Int, Int)]
  print $ part1 dataPoints
  print $ part2 dataPoints
