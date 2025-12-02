import Data.List (nub)
import System.Environment (getArgs)

data Problem =
  Problem Int Int
  deriving (Show)

instance Read Problem where
  readsPrec _ s = do
    let (a, rest) = span (/= '-') s
    ('-':rest') <- [rest]
    let (b, rest'') = span (\x -> (x /= ',') && (x /= '\n')) rest'
    return (Problem (read a) (read b), rest'')

ans1 :: Problem -> [Int]
ans1 (Problem a b) = do
  let showa = show a
  let xparts' = take (length showa `div` 2) showa
  let xpart =
        if xparts' == ""
          then "1"
          else xparts'
  let candidates = do
        xparti <- [read xpart ..] :: [Int]
        return $ read (show xparti ++ show xparti)
  filter (>= a) $ takeWhile (<= b) candidates

ans2 :: Problem -> [Int]
ans2 (Problem a b) = do
  let showa = show a
  repeatCount <- [2 .. length (show b)]
  let xparts' = take (length showa `div` repeatCount) showa
  let xpart =
        if xparts' == ""
          then "1"
          else xparts'
  let candidates = do
        xparti <- [read xpart ..] :: [Int]
        return $ read $ concat $ replicate repeatCount (show xparti)
  filter (>= a) $ takeWhile (<= b) candidates

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc2.in"
          (x:_) -> x
  datal <- readFile filename
  let probs = read ("[" ++ datal ++ "]") :: [Problem]
  print $ sum $ nub $ concatMap ans1 probs
  print $ sum $ nub $ concatMap ans2 probs
