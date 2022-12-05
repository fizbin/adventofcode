import System.Environment
import Data.List.Split
import Data.Char (isDigit)
import Control.Monad.State.Strict

mkStacks :: [String] -> [String]
mkStacks (s:_) | '[' `notElem` s = []
mkStacks [] = []
mkStacks (s:ss) = let underneath = mkStacks ss
                      myadditions = map fst $ filter snd $ zip s (cycle [False, True, False, False])
                   in case underneath of
                    [] -> map (:[]) myadditions
                    a -> zipWith (\c s -> if c == ' ' then s else c:s) myadditions a

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx newval lst = take idx lst ++ [newval] ++ drop (idx+1) lst

mvStacks :: Bool -> String -> State [String] ()
mvStacks doRev order  = let order' = map (\c -> if isDigit c then c else ' ') order
                            (n, order''):_ = reads order'
                            (von, order'''):_ = reads order''
                            zu = read order'''
                        in do
                            oldvon <- gets (!! (von - 1))
                            oldzu <- gets (!! (zu - 1))
                            let newvon = drop n oldvon
                            let newzu = (if doRev then reverse else id) (take n oldvon) ++ oldzu
                            modify' (replaceAt (von-1) newvon)
                            modify' (replaceAt (zu-1) newzu)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc5.in" else head args
    s <- lines <$> readFile filename
    let (stackspec:orders:_) = splitWhen null s
    let stacks = mkStacks stackspec
    -- print stacks
    let retval = execState (mapM (mvStacks True) orders) stacks
    print $ head <$> retval
    let retval' = execState (mapM (mvStacks False) orders) stacks
    print $ head <$> retval'