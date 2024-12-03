import System.Environment (getArgs)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Data.Functor (($>))
import Control.Monad.State

data Instruction = Do | Dont | Mul Int Int deriving Show

newtype InstructionList = IL [Instruction] deriving Show

parseInstr :: ReadP Instruction
parseInstr = (string "do()" $> Do) +++ (string "don't()" $> Dont)
             +++ (Mul <$> (string "mul(" *> (read <$> many1 (satisfy isDigit)))
                      <*> (string "," *> (read <$> many1 (satisfy isDigit))) <* string ")")

parseIL :: ReadP InstructionList
parseIL = IL <$> parsy
  where
    parsy = concat <$> manyTill (((:[]) <$> parseInstr) <++ (satisfy (const True) $> [])) eof

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
    let myParser = readP_to_S parseIL
    let myIL = fst $ head $ filter (null . snd) $ myParser mydata
    putStr "Part 1: "
    print $ eval1 myIL
    putStr "Part 2: "
    print $ eval2 myIL
