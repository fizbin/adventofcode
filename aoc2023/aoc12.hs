import Data.Array as A
import Data.Char (isSpace)
import Data.Function (fix)
import Data.List (intercalate, isSuffixOf, tails)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

findCombos :: ([Char] -> [Int] -> Int) -> [Char] -> [Int] -> Int
findCombos _ s [] = if '#' `elem` s then 0 else 1
findCombos _ [] _ = 0
findCombos recurse (s0 : ss) qa@(q0 : qs) =
        case s0 of
                '.' -> recurse ss qa
                '#' -> poundCase ss q0 qs
                '?' -> recurse ss qa + poundCase ss q0 qs
                _ -> error $ "unknown character " ++ [s0]
    where
        poundCase srest q qrest
                | q - 1 > length srest = 0
                | q - 1 == length srest = if ('.' `elem` srest) || not (null qrest) then 0 else 1
                | otherwise =
                        sum $
                                [ x
                                | (header, dot : rest) <- [splitAt (q - 1) srest]
                                , dot /= '#'
                                , '.' `notElem` header
                                , let x = recurse rest qrest
                                ]

processLine1 :: String -> Int
processLine1 str =
        let (fstr, seqnumsStr) = break isSpace str
            seqnums = read <$> splitOn "," seqnumsStr
         in fix findCombos fstr seqnums

-- make a version of findCombos tied to a particular full string
comboCafFor :: [Char] -> [Int] -> [Char] -> [Int] -> Int
comboCafFor fullFixRecord fullSeqNums = tiedFindCombos
    where
        tiedArray =
                array
                        ((0, 0), (length fullFixRecord, length fullSeqNums))
                        [((length fr, length sn), tiedFindCombos fr sn) | fr <- tails fullFixRecord, sn <- tails fullSeqNums]
        guardedLookup fr sn =
                if (fr `isSuffixOf` fullFixRecord) && (sn `isSuffixOf` fullSeqNums)
                        then tiedArray A.! (length fr, length sn)
                        else error ("guardedLookup: " ++ show (fr, sn) ++ " when full was " ++ show (fullFixRecord, fullSeqNums))
        tiedFindCombos = findCombos guardedLookup

processLine2 :: String -> Int
processLine2 str =
        let (fstr, seqnumsStr) = break isSpace str
            seqnums = read <$> splitOn "," seqnumsStr
            fullFstr = intercalate "?" $ replicate 5 fstr
            fullSeqNums = concat $ replicate 5 seqnums
         in comboCafFor fullFstr fullSeqNums fullFstr fullSeqNums

main :: IO ()
main = do
        args <- getArgs
        let filename = if null args then "aoc12.in" else head args
        s <- lines <$> readFile filename
        print $ sum $ processLine1 <$> s
        print $ sum $ processLine2 <$> s
