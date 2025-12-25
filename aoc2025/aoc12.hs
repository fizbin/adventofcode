import Data.Char (isDigit, isSpace)
import Data.Map.Strict qualified as M
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP qualified as P
import Control.Monad (void)

data Input = Input
  { pieces :: M.Map Int [[Char]],
    puzzles :: [((Int, Int), [Int])]
  } deriving (Show)

readPInt :: P.ReadP Int
readPInt = read <$> (P.skipSpaces *> P.munch1 isDigit)

readPch :: Char -> P.ReadP ()
readPch ch = void (P.satisfy (== ch))

readPInput :: P.ReadP Input
readPInput = do
  pieces <- P.many1 $ do
    idval <- readPInt
    readPch ':'
    readPch '\n'
    rows <- P.many1 $ P.munch1 (not . isSpace) <* readPch '\n'
    readPch '\n'
    pure (idval, rows)
  puzzles <- P.many1 $ do
    width <- readPInt
    readPch 'x'
    height <- readPInt
    readPch ':'
    lst <- P.sepBy1 readPInt (readPch ' ')
    readPch '\n'
    pure ((width, height), lst)
  P.skipSpaces
  pure $ Input (M.fromList pieces) puzzles

instance Read Input where
    readsPrec _ = P.readP_to_S readPInput

canFit ::  M.Map Int [[Char]] -> ((Int, Int), [Int]) -> Bool
canFit pieces ((width,height), puzzles)
  | easyFit = True
  | easyReject = False
  | otherwise = undefined
  where
    puzzleSizes :: M.Map Int Int
    puzzleSizes = M.map (sum . concatMap (map (\x -> if x == '#' then 1 else 0))) pieces
    totalRequiredSpace = sum $ do
        (pid, count) <- zip [0..] puzzles
        pure $ count * (puzzleSizes M.! pid)
    easyReject = width*height < totalRequiredSpace
    maxWidth = maximum $ do
        rows <- M.elems pieces
        pure $ maximum $ length <$> rows
    maxHeight = maximum $ do
        rows <- M.elems pieces
        pure $ length rows
    easyFit = sum puzzles <= (width `div` maxWidth) * (height `div` maxHeight)

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc12.in"
          (x : _) -> x
  input <- (read <$> readFile filename) :: IO Input
  print $ length $ filter (canFit (pieces input)) (puzzles input)
