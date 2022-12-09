import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.State
import qualified Data.Set as S
import System.Environment (getArgs)

nextSpot :: (Int, Int) -> (Int, Int) -> (Int, Int)
nextSpot (hd1, hd2) (tl1, tl2)
  | (abs (hd1 - tl1) <= 1) && (abs (hd2 - tl2) <= 1) = (tl1, tl2)
nextSpot (hd1, hd2) (tl1, tl2) = (tl1 + signum (hd1 - tl1), tl2 + signum (hd2 - tl2))

-- state is where everything is, retval is where the last thing goes
dorule :: String -> State [(Int, Int)] [(Int, Int)]
dorule line = do
  let d = head line
  let m = (read $ tail line) :: Int
  replicateM m $ do
    hd <- gets head
    let hd' =
          case d of
            'U' -> first (+ 1) hd
            'D' -> first pred hd
            'L' -> second succ hd
            'R' -> second pred hd
            x -> error ("Bad dir: " ++ [x])
    snake <- gets tail
    let newsnake = scanl nextSpot hd' snake
    put newsnake
    pure $ last newsnake

main :: IO ()
main = do
  args <- getArgs
  let filename =
        if null args
          then "aoc9.in"
          else head args
  s <- lines <$> readFile filename
  -- let s = lines "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2\n"
  let p1Set = S.fromList $ join $ flip evalState [(0, 0), (0, 0)] $ forM s dorule
  print $ S.size p1Set
  let p2Set = S.fromList $ join $ flip evalState (replicate 10 (0, 0)) $ forM s dorule
  print $ S.size p2Set
