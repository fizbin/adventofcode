import Control.Monad (foldM, unless)
import Control.Monad.State (MonadState(put), runState)
import qualified Data.Map.Strict as M
import System.Environment (getArgs)

deleteRemovables :: M.Map (Int, Int) Char -> (M.Map (Int, Int) Char, Bool)
deleteRemovables grid =
  flip runState False $ do
    let removeAbles = [s | (s, v) <- M.toList grid, v == '@', nbCount s < 4]
    unless (null removeAbles) $ put True
    foldM (\mp k -> pure (M.insert k 'x' mp)) grid removeAbles
  where
    nbCount (x, y) =
      sum
        $ [ (\s -> fromEnum ('@' == M.findWithDefault ' ' s grid)) (x', y')
          | x' <- [x - 1, x, x + 1]
          , y' <- [y - 1, y, y + 1]
          , (x' /= x) || (y' /= y)
          ]

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc4.in"
          (x:_) -> x
  gridData <- readFile filename
  let grid =
        M.fromList $ do
          (x, row) <- zip [(0 :: Int) ..] (lines gridData)
          (y, ch) <- zip [(0 :: Int) ..] row
          return ((x, y), ch)
  let grid2 = fst $ deleteRemovables grid
  let countSpot '@' = 1 :: Int
      countSpot _ = 0
  let initial = sum (map countSpot $ M.elems grid)
  print (initial - sum (map countSpot $ M.elems grid2))
  let removeAll gr =
        case deleteRemovables gr of
          (x, True) -> removeAll x
          (x, False) -> x
  print (initial - sum (map countSpot $ M.elems $ removeAll grid2))
