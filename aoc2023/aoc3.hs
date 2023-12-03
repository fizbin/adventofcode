import Control.Monad (guard, join)
import Data.Char (isDigit)
import Data.List (tails)
import Data.Map.Strict qualified as M
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        let filename = if null args then "aoc3.in" else head args
        -- need to replace '.' with something else because the reads instance for Int
        -- will return [] when handed "5.4" but will return [(5, "x4")] when handed "5x4".
        -- Can't use space instead of dot because skipping leading spaces would also be bad
        s0 <- lines . map (\ch -> if ch == '.' then 'x' else ch) <$> readFile filename
        let s =
                [replicate (2 + length (head s0)) 'x']
                        ++ ["x" ++ ln ++ "x" | ln <- s0]
                        ++ [replicate (2 + length (head s0)) 'x']
        let chMap = M.fromList $ do
                (rowidx, row) <- zip [0 :: Int ..] s
                (colidx, ch) <- zip [0 :: Int ..] row
                pure ((rowidx, colidx), ch)
        let goodnums = do
                (rowidx, row) <- zip [0 :: Int ..] s
                (pstart : start) <- tails row
                guard $ not (isDigit pstart)
                let colidx = length row - length start
                (num, rest) <- reads start
                let numlen = length start - length rest
                let above = take (2 + numlen) $ drop (colidx - 1) (s !! (rowidx - 1))
                let below = take (2 + numlen) $ drop (colidx - 1) (s !! (rowidx + 1))
                let neighbors = [pstart, head rest]
                guard $ not (all isDigit (filter (/= 'x') $ above ++ below ++ neighbors))
                pure (num + 0 :: Int)
        print $ sum goodnums
        let asternums = do
                (rowidx, row) <- zip [0 :: Int ..] s
                (pstart : start) <- tails row
                guard $ not (isDigit pstart)
                let colidx = length row - length start
                (num, rest) <- reads start
                let numlen = length start - length rest
                let asterAboveBelow =
                        [ [(rowidx - 1, col), (rowidx + 1, col)]
                        | col <- [colidx - 1 .. colidx + numlen]
                        ]
                (asteri, asterj) <-
                        [(rowidx, colidx - 1), (rowidx, colidx + numlen)]
                                ++ join asterAboveBelow
                guard $ M.lookup (asteri, asterj) chMap == Just '*'
                pure ((asteri, asterj), [num + 0 :: Int])
        let asterMap = M.fromListWith (++) asternums
        print $ sum $ map (\case [x, y] -> x * y; _ -> 0) $ M.elems asterMap
