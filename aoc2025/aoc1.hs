import Data.List (mapAccumL)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc1.in"
          (x:_) -> x
  datal <- lines <$> readFile filename
  let parseLine (x:xs) = (x, read xs)
      parseLine _ = error "invalid line"
  let instructions = map parseLine datal
  let movement ('L', n) = n
      movement ('R', n) = -n
      movement _ = error "invalid instruction"
  let sz = 100 :: Int
  let movements = map movement instructions
  let acc1 dial x =
        let newPos = (dial + x) `mod` sz
         in (newPos, fromEnum $ newPos == 0)
  let answer1 :: Int
      answer1 = sum $ snd $ mapAccumL acc1 50 movements
  print answer1
  let acc2 dial x =
        let newPos = (dial + x) `mod` sz
            baseMove = abs x `div` sz
            extraMove =
              case ( compare dial 0
                   , compare newPos 0
                   , compare x 0
                   , compare newPos dial) of
                (EQ, _, _, _) -> 0
                (_, EQ, _, _) -> 1
                (_, _, GT, LT) -> 1
                (_, _, LT, GT) -> 1
                _ -> 0
         in (newPos, baseMove + extraMove)
  let answer2 :: Int
      answer2 = sum $ snd $ mapAccumL acc2 50 movements
  print answer2
