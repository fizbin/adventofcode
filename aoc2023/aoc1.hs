import Control.Applicative
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc1.in"
                else head args
    s <- lines <$> readFile filename
    let findDigit (d : _) = if isDigit d then Just [d] else Nothing
        findDigit [] = Nothing
    print $ sum $ map (\ln -> (read (getFirst findDigit ln ++ getLast findDigit ln) :: Int)) s
    print $ sum $ map (\ln -> (read (getFirst exDigit ln ++ getLast exDigit ln) :: Int)) s

getLast :: ([Char] -> Maybe [Char]) -> String -> [Char]
getLast f s0 = fromMaybe [] (getLast' s0)
  where
    getLast' [] = f []
    getLast' a@(_ : r) = getLast' r <|> f a

getFirst :: ([Char] -> Maybe [Char]) -> String -> [Char]
getFirst f s0 = fromMaybe [] (getFirst' s0)
  where
    getFirst' [] = f []
    getFirst' a@(_ : r) = f a <|> getFirst' r

digits :: [(String, String)]
digits = zip (splitOn "," "one,two,three,four,five,six,seven,eight,nine") (map show [(1::Int) .. 9])

exDigit :: String -> Maybe String
exDigit [] = Nothing
exDigit a@(c : _) =
    if isDigit c
        then Just [c]
        else listToMaybe [y | (x, y) <- digits, x `isPrefixOf` a]