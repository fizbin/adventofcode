import Control.Monad (forM_, void)
import Control.Monad.ST (ST, runST)
import Data.Char (isDigit)
import Data.List (tails)
import Data.Map.Strict qualified as M
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Dijkstra (dijkstra')
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

data Machine = Machine
  { lights :: [Bool]
  , buttons :: [[Int]]
  , jolts :: [Int]
  } deriving (Show)

readPch :: Char -> ReadP ()
readPch ch = void (P.satisfy (== ch))

intReadP :: ReadP Int
intReadP = read <$> P.munch1 isDigit

machineParser :: ReadP Machine
machineParser = do
  readPch '['
  lightstr <- P.munch1 (\x -> (x == '.') || (x == '#'))
  readPch ']'
  let lights = map (== '#') lightstr
  buttons <-
    P.many1 $ do
      P.skipSpaces
      readPch '('
      bspec <- P.sepBy intReadP (readPch ',')
      readPch ')'
      pure bspec
  P.skipSpaces
  readPch '{'
  jspec <- P.sepBy intReadP (readPch ',')
  readPch '}'
  P.skipSpaces
  pure $ Machine lights buttons jspec

instance Read Machine where
  readsPrec :: Int -> ReadS Machine
  readsPrec _ = P.readP_to_S machineParser

powerSet :: [a] -> [[a]]
powerSet lst = concatMap (nOf lst) [0 .. length lst]
  where
    nOf :: [t] -> Int -> [[t]]
    nOf _ 0 = [[]]
    nOf [] _ = []
    nOf l x
      | x > length l = []
    nOf (x:xs) i = map (x :) (nOf xs (i - 1)) ++ nOf xs i

part1 :: Machine -> Int
part1 machine =
  case workingCombos of
    [] -> error $ "Couldn't solve lights on " ++ show machine
    (x:_) -> length x
  where
    buttonCombos = powerSet $ buttons machine
    res buttonCombo =
      let ans =
            flip map [0 .. length (lights machine) - 1] $ \k ->
              M.findWithDefault False k
                $ M.fromListWith (/=) (map (, True) $ concat buttonCombo)
       in ans -- trace (show buttonCombo ++ " -> " ++ show ans) ans
    workingCombos = filter (\c -> res c == lights machine) buttonCombos

data SearchState = S
  { unpressed :: [[Int]]
  , stepSize :: Int
  , remaining :: [Int]
  } deriving (Eq, Ord)

acceptFinal :: SearchState -> Bool
acceptFinal (S {remaining = r}) = all (== 0) r

acceptableState :: SearchState -> Bool
acceptableState (S {stepSize = sz, remaining = r}) =
  all (>= 0) r && all (\x -> x `mod` sz == 0) r

removeButton :: Int -> [Int] -> [Int] -> [Int]
removeButton mult button remaining = U.toList $ runST doit
  where
    doit :: forall s. ST s (U.Vector Int)
    doit = do
      mvector <- U.thaw $ U.fromList remaining
      forM_ button $ \b -> UM.modify mvector (\x -> x - mult) b
      U.freeze mvector

-- ~30s
transitions :: Machine -> SearchState -> [(SearchState, Int)]
transitions machine (S {unpressed = bs, stepSize = sz, remaining = r}) =
  filter (acceptableState . fst)
    $ [(S bs' sz r', sz) | (b:bs') <- tails bs, let r' = removeButton sz b r]
        ++ [(S (buttons machine) (sz * 2) r, 0)]

-- ~25s
transitions' :: Machine -> SearchState -> [(SearchState, Int)]
transitions' machine (S {stepSize = sz, remaining = r}) =
  filter (acceptableState . fst)
    $ [ (S (buttons machine) (sz * 2) r', sz * length pressed)
      | pressed <- powerSet (buttons machine)
      , let r' = foldr (removeButton sz) r pressed
      ]

-- ~0.366s
transitions'' :: Machine -> SearchState -> [(SearchState, Int)]
transitions'' machine = doit
  where
    sigMap =
      M.fromListWith (++) $ do
        bCombo <- powerSet (buttons machine)
        let r' =
              foldr
                (removeButton 1)
                (replicate (length $ jolts machine) 0)
                bCombo
        let sig = map odd r'
        pure (sig, [bCombo])
    doit (S {stepSize = sz, remaining = r}) =
      filter (acceptableState . fst)
        $ [ (S (buttons machine) (sz * 2) r', sz * length pressed)
          | pressed <- M.findWithDefault [] (map (odd . (`div` sz)) r) sigMap
          , let r' = foldr (removeButton sz) r pressed
          ]

part2 :: (Machine -> SearchState -> [(SearchState, Int)]) -> Machine -> Int
part2 txnFunc machine =
  case dijkstra'
         (txnFunc machine)
         (S (buttons machine) 1 (jolts machine))
         acceptFinal of
    Just (_, d) -> d
    Nothing -> error $ "No solution found for " ++ show machine

main :: IO ()
main = do
  args <- getArgs
  let (txnFunc, rargs) =
        case args of
          ("-t0":s) -> (transitions, s)
          ("-t1":s) -> (transitions', s)
          ("-t2":s) -> (transitions'', s)
          _ -> (transitions'', args)
  let filename =
        case rargs of
          [] -> "aoc10.in"
          (x:_) -> x
  dataLines <- lines <$> readFile filename
  let machines = map read dataLines :: [Machine]
  print $ sum $ map part1 machines
  print $ sum $ map (part2 txnFunc) machines
