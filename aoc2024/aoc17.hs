import Control.Monad (guard)
import Data.Bits
import Data.Char (isDigit)
import Data.List (intercalate, nub)
import System.Environment (getArgs)

findints :: String -> [Int]
findints "" = []
findints ('-' : x : xs) | isDigit x = case findints (x : xs) of
  [] -> error "Can't happen"
  (n : ns) -> (-n) : ns
findints (x : xs) | not (isDigit x) = findints xs
findints s = let (n, s') = head (reads s) in n : findints s'

data Machine = Machine
  { regA :: Int,
    regB :: Int,
    regC :: Int,
    iPointer :: Int
  }
  deriving (Show)

getCombo :: Machine -> Int -> Int
getCombo _ val | val < 4 = val
getCombo m 4 = regA m
getCombo m 5 = regB m
getCombo m 6 = regC m
getCombo _ x = error ("Bad combo value " ++ show x)

runInstruction :: Int -> Int -> Machine -> (Maybe Int, Machine)
runInstruction 0 arg m = (Nothing, m {regA = regA m `div` (2 ^ getCombo m arg), iPointer = 2 + iPointer m})
runInstruction 1 arg m = (Nothing, m {regB = regB m .^. arg, iPointer = 2 + iPointer m})
runInstruction 2 arg m = (Nothing, m {regB = getCombo m arg `mod` 8, iPointer = 2 + iPointer m})
runInstruction 3 arg m = (Nothing, if regA m == 0 then m {iPointer = 2 + iPointer m} else m {iPointer = arg})
runInstruction 4 _ m = (Nothing, m {regB = regB m .^. regC m, iPointer = 2 + iPointer m})
runInstruction 5 arg m = (Just (getCombo m arg `mod` 8), m {iPointer = 2 + iPointer m})
runInstruction 6 arg m = (Nothing, m {regB = regA m `div` (2 ^ getCombo m arg), iPointer = 2 + iPointer m})
runInstruction 7 arg m = (Nothing, m {regC = regA m `div` (2 ^ getCombo m arg), iPointer = 2 + iPointer m})
runInstruction _ _ _ = error "Bad instruction"

runprog :: [Int] -> Machine -> [Int]
runprog prog !m =
  if iPointer m < 0 || iPointer m >= length prog - 1
    then []
    else
      let instr = prog !! iPointer m
          arg = prog !! (1 + iPointer m)
          (outval, m') = runInstruction instr arg m
       in case outval of
            Just o -> o : runprog prog m'
            Nothing -> runprog prog m'

mkQuine :: [Int] -> Int
mkQuine prog = go 0 [0]
  where
    go ncorrect [] = error $ "Out of possibilities at length " ++ show ncorrect
    go ncorrect aPoss | ncorrect == length prog = minimum aPoss
    go ncorrect aPoss =
      -- we trust the bottom 3*ncorrect bits of aPoss
      let newAval = do
            base <- nub $ map (`mod` (2 ^ (3 * ncorrect))) aPoss
            increment <- [0 .. 1023]
            let aval = increment * (2 ^ (3 * ncorrect)) .^. (base `mod` (2 ^ (3 * ncorrect)))
            let runM = runprog prog (Machine {regA = aval, regB = 0, regC = 0, iPointer = 0})
            guard $ take (ncorrect + 1) runM == take (ncorrect + 1) prog
            pure aval
       in go (ncorrect + 1) newAval

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc17.in" else head args
  mydata <- findints <$> readFile filename
  let ainit = head mydata
      binit = mydata !! 1
      cinit = mydata !! 2
      prog = drop 3 mydata
  let iMachine = Machine {regA = ainit, regB = binit, regC = cinit, iPointer = 0}
  putStrLn $ "Part 1: " ++ intercalate "," (map show $ runprog prog iMachine)
  putStrLn $ "Part 2: " ++ show (mkQuine prog)
