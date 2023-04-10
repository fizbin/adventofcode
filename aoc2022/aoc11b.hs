import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable (Foldable(foldl'))
import Data.List (sort)
import Data.List.Split
import Debug.Trace
import System.Environment (getArgs)
import Control.Arrow (Arrow(first))

data Monkey =
    Monkey
        { mkNum :: Int
        , mkItems :: [Integer]
        , mkOp :: Integer -> Integer
        , mkTDiv :: Int
        , mkDestT :: Int
        , mkDestF :: Int
        }

parseMonkey :: [String] -> Monkey
parseMonkey [numline, itline, opline, testline, trueline, falseline] =
    let tmkNum = read $ takeWhile isDigit $ dropWhile (not . isDigit) numline
        tmkItems = read $ "[ " ++ dropWhile (not . isDigit) itline ++ " ]"
        (_:opend:_) = splitOn "= old " opline
        opArg op "old" = \x -> x `op` x
        opArg op " old" = \x -> x `op` x
        opArg op s = let rval = read s in (`op` rval)
        mkOp ('*':r) = opArg (*) r
        mkOp ('+':r) = opArg (+) r
        mkOp oth = error $ "bad opspec: " ++ oth
        tmkOp = mkOp opend
        tmkTDiv = read $ dropWhile (not . isDigit) testline
        tmkDestT = read $ dropWhile (not . isDigit) trueline
        tmkDestF = read $ dropWhile (not . isDigit) falseline
     in Monkey tmkNum tmkItems tmkOp tmkTDiv tmkDestT tmkDestF
parseMonkey x = error $ "Bad spec length " ++ show x

addItem :: Monkey -> Integer -> Monkey
addItem monkey it =
    let newits = mkItems monkey ++ [it]
     in last newits `seq` monkey {mkItems = newits}

swapItem :: Int -> a -> [a] -> [a]
swapItem idx monk monklist = a `seq` b `seq` c
  where
    a = take idx monklist
    b = a ++ [monk]
    c = b ++ drop (idx + 1) monklist

runMonkey :: (Integer -> Integer) -> Monkey -> State ([Monkey], [Int]) ()
runMonkey manager monkey = do
    let monkNum = mkNum monkey
    forM_ (mkItems monkey) $ \it -> do
        let it' = mkOp monkey it
        let it'' = manager it'
        let destMN =
                if it'' `mod` toInteger (mkTDiv monkey) == 0
                    then mkDestT monkey
                    else mkDestF monkey
        oldMonkeys <- gets fst
        let newMonkeys = swapItem destMN (addItem (oldMonkeys !! destMN) it'') oldMonkeys
        modify' (first $ const newMonkeys)
    (oldMonkeys, oldIcount) <- get
    let newIcount = swapItem monkNum (length (mkItems monkey) + (oldIcount !! monkNum)) oldIcount
    oldMonkeys <- gets fst
    let newMonkeys = swapItem monkNum (monkey {mkItems = []}) oldMonkeys
    put (newMonkeys, newIcount)

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc11.in"
                else head args
    s <- lines <$> readFile filename
    let monkeyspecs = splitWhen null s
    let monkeys = map parseMonkey monkeyspecs
    let (_, finalCounts) =
            flip execState (monkeys, replicate (length monkeys) 0) $
            replicateM_ 20 $ do
                nmonks <- length <$> gets fst
                forM [0 .. nmonks - 1] $ \idx -> do
                    cmonks <- gets fst
                    runMonkey (`div` 3) (cmonks !! idx)
    print $ product $ take 2 $ reverse $ sort finalCounts
    let bigmod = product $ map (toInteger . mkTDiv) monkeys
    let (_, finalCounts') =
            flip execState (monkeys, replicate (length monkeys) 0) $
            replicateM_ 10000 $ do
                nmonks <- length <$> gets fst
                forM [0 .. nmonks - 1] $ \idx -> do
                    cmonks <- gets fst
                    runMonkey (`mod` bigmod) (cmonks !! idx)
    print $ product $ take 2 $ reverse $ sort finalCounts'
