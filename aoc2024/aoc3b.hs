import System.Environment (getArgs)
import Control.Monad.State
data Instruction = Do | Dont | Mul Int Int deriving Show

newtype InstructionList = IL [Instruction] deriving Show

parseInstr :: String -> (Maybe Instruction, String)
parseInstr ('d':'o':'(':')':s) = (Just Do, s)
parseInstr ('d':'o':'n':'\'':'t':'(':')':s) = (Just Dont, s)
parseInstr ('m':'u':'l':'(':s) =
    let trialParse = [(Mul a b, s'') | (a, ',':s') <- reads s, (b, ')':s'') <- reads s']
    in case trialParse of
        (mulI, rst):_ -> (Just mulI, rst)
        _ -> (Nothing, s)
parseInstr [] = (Nothing, [])
parseInstr s = (Nothing, tail s)

parseIL :: String -> InstructionList
parseIL = IL . parsy
  where
    parsy [] = []
    parsy s = case parseInstr s of
        (Just i, s') -> i : parsy s'
        (Nothing, s') -> parsy s'

eval1 :: InstructionList -> Int
eval1 (IL instrs) = sum $ doer <$> instrs
  where
    doer Do = 0
    doer Dont = 0
    doer (Mul a b) = a * b

eval2 :: InstructionList -> Int
eval2 (IL instrs) = snd $ execState (mapM_ doer instrs) (True, 0)
  where
    doer :: Instruction -> State (Bool, Int) ()
    doer Do = modify (\s -> (True, snd s))
    doer Dont = modify (\s -> (False, snd s))
    doer (Mul a b) = modify (\s -> (fst s, snd s + if fst s then a*b else 0))

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc3.in" else head args
    mydata <- readFile filename
    let myIL = parseIL mydata
    putStr "Part 1: "
    print $ eval1 myIL
    putStr "Part 2: "
    print $ eval2 myIL
