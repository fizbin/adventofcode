import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable (Foldable(foldl'))
import Data.List (sort)
import Data.List.Split
import Debug.Trace
import System.Environment (getArgs)

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
        mkOp ['*', ' ', 'o', 'l', 'd'] = \x -> x * x
        mkOp ('*':r) = (*) (read r)
        mkOp ('+':r) = (+) (read r)
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

runMonkey1 :: Monkey -> State ([Monkey], [Int]) ()
runMonkey1 monkey = do
    let monkNum = mkNum monkey
    forM_ (mkItems monkey) $ \it -> do
        let it' = mkOp monkey it
        let it'' = it' `div` 3
        let destMN =
                if it'' `mod` toInteger (mkTDiv monkey) == 0
                    then mkDestT monkey
                    else mkDestF monkey
        -- traceM $ "Monkey #" ++ (show monkNum) ++ " working with item that started as " ++ show it ++ " but is now " ++ show it'' ++ " going to monkey " ++ show destMN 
        oldMonkeys <- gets fst
        let newMonkeys = swapItem destMN (addItem (oldMonkeys !! destMN) it'') oldMonkeys
        modify' (\(_, ic) -> (newMonkeys, ic))
    oldIcount <- gets snd
    let newIcount = swapItem monkNum (length (mkItems monkey) + (oldIcount !! monkNum)) oldIcount
    modify' (\(m, _) -> (m, newIcount))
    oldMonkeys <- gets fst
    let newMonkeys = swapItem monkNum (monkey {mkItems = []}) oldMonkeys
    -- traceM $ "New monkeys with items: " ++ show (map mkItems newMonkeys)
    modify' (\(_, ic) -> (newMonkeys, ic))

runMonkey2 :: Integer -> Monkey -> State ([Monkey], [Int]) ()
runMonkey2 bigmod monkey = do
    let monkNum = mkNum monkey
    forM_ (mkItems monkey) $ \it -> do
        let it' = mkOp monkey it
        let it'' = it' `mod` bigmod
        let destMN =
                if it'' `mod` toInteger (mkTDiv monkey) == 0
                    then mkDestT monkey
                    else mkDestF monkey
        -- traceM $ "Monkey #" ++ (show monkNum) ++ " working with item that started as " ++ show it ++ " but is now " ++ show it'' ++ " going to monkey " ++ show destMN 
        oldMonkeys <- gets fst
        let newMonkeys = swapItem destMN (addItem (oldMonkeys !! destMN) it'') oldMonkeys
        modify' (\(_, ic) -> (newMonkeys, ic))
    oldIcount <- gets snd
    let newIcount = swapItem monkNum (length (mkItems monkey) + (oldIcount !! monkNum)) oldIcount
    modify' (\(m, _) -> (m, newIcount))
    oldMonkeys <- gets fst
    let newMonkeys = swapItem monkNum (monkey {mkItems = []}) oldMonkeys
    -- traceM $ "New monkeys with items: " ++ show (map mkItems newMonkeys)
    modify' (\(_, ic) -> (newMonkeys, ic))

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
    -- print $ map (`mkOp` 10) monkeys
    -- print $ map mkNum monkeys
    -- print $ map mkDestT monkeys
    -- print $ map mkDestF monkeys
    -- print $ map mkItems monkeys
    -- print $ map mkTDiv monkeys
    let (_, finalCounts) =
            flip execState (monkeys, replicate (length monkeys) 0) $
            replicateM_ 20 $ do
                nmonks <- length <$> gets fst
                forM [0 .. nmonks - 1] $ \idx -> do
                    cmonks <- gets fst
                    runMonkey1 (cmonks !! idx)
    -- print $ map mkItems monks
    print $ (\x -> head x * head (tail x)) $ reverse $ sort finalCounts
    let bigmod = product $ map (toInteger . mkTDiv) monkeys
    -- print bigmod
    let (_, finalCounts') =
            flip execState (monkeys, replicate (length monkeys) 0) $
            replicateM_ 10000 $ do
                nmonks <- length <$> gets fst
                forM [0 .. nmonks - 1] $ \idx -> do
                    cmonks <- gets fst
                    runMonkey2 bigmod (cmonks !! idx)
    print $ (\x -> head x * head (tail x)) $ reverse $ sort finalCounts'
