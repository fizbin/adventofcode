import Data.Char (isAsciiLower, isAsciiUpper, ord)
import Data.Set (findMin, fromList, intersection)
import System.Environment

--import Debug.Trace
priority :: Char -> Int
priority x
    | isAsciiLower x = 1 + ord x - ord 'a'
priority x
    | isAsciiUpper x = 27 + ord x - ord 'A'
priority _ = error "Priority asked of bad value"

scores1 = map comm
  where
    comm s =
        let (p1, p2) = splitAt (length s `div` 2) s
            p1s = fromList p1
            p2s = fromList p2
            inter = intersection p1s p2s
         in priority $ findMin inter

scores2 [] = []
scores2 (x:y:z:s) = doone x y z : scores2 s
  where
    doone x y z =
        priority $ findMin $ intersection (fromList x) $ intersection (fromList y) (fromList z)
scores2 _ = error "scores2 asked of bad value; list not multiple of 3?"

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc3.in"
                else head args
    s <- lines <$> readFile filename
    print $ sum $ scores1 s
    print $ sum $ scores2 s
