import Data.Array
import Data.Char (ord)
import Data.List (foldl', isSuffixOf)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

hashStr :: String -> Int
hashStr = foldl' (\n c -> ((n + ord c) * 17) `mod` 256) 0

type AOCHash = Array Int [(String, Int)]
delval :: String -> AOCHash -> AOCHash
delval lbl arr =
    let hlbl = hashStr lbl
     in arr // [(hlbl, [ent | ent <- arr ! hlbl, fst ent /= lbl])]

insval :: String -> Int -> AOCHash -> AOCHash
insval lbl val arr =
    let hlbl = hashStr lbl
        oldbox = arr ! hlbl
        newbox =
            if any ((== lbl) . fst) oldbox
                then [(l, v') | (l, v) <- oldbox, let v' = if l == lbl then val else v]
                else oldbox ++ [(lbl, val)]
     in arr // [(hlbl, newbox)]

handleinstr :: String -> AOCHash -> AOCHash
handleinstr instr =
    let (lbl, vals) = break (== '=') instr
     in if "-" `isSuffixOf` instr then delval (init instr) else insval lbl (read $ tail vals)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc15.in" else head args
    s <- filter (/= '\n') <$> readFile filename
    let instrs = splitOn "," s
    print $ sum $ map hashStr instrs
    let finarr = foldl' (flip handleinstr) (listArray (0, 255) (replicate 256 [])) instrs
    let boxsig box = sum $ zipWith ((*) . snd) box [1 ..]
    let sig = sum $ zipWith ((*) . boxsig) (elems finarr) [1 ..]
    print sig
