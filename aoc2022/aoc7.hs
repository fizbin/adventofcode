import Control.Monad.State.Strict
import Data.Bifunctor (first, second)
import Data.List
import qualified Data.Map.Strict as M
import System.Environment (getArgs)

process :: String -> State ([String], M.Map [String] Int) ()
process "$ ls" = pure ()
process "$ cd .." = modify (first tail)
process x
    | "$ cd " `isPrefixOf` x =
        let newdir = words x !! 2
         in modify (first (newdir :))
process x
    | "dir " `isPrefixOf` x = do
        let newdir = words x !! 1
        cwd <- gets fst
        modify $ second $ M.insert (newdir : cwd) 0
process x = do
    let [sizestr, filename] = words x
    let size = read sizestr
    cwd <- gets fst
    forM_ (tails cwd) $ \dir -> do
        currval <- gets ((M.! dir) . snd)
        modify $ second $ M.insert dir (currval + size)

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc7.in"
                else head args
    s <- tail . lines <$> readFile filename
    let (_, dir_size_map) = execState (mapM_ process s) ([], M.singleton [] 0)
    print $ sum $ filter (<= 100000) $ map snd $ M.toList dir_size_map
    let remaining = 70000000 - (dir_size_map M.! [])
    let needed = 30000000 - remaining
    print $ minimum $ filter (>= needed) $ map snd $ M.toList dir_size_map
