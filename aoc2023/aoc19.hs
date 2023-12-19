import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Functor (($>))
import Data.List.Split (splitOn)
import Data.Map qualified as M
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP

data RuleSpec = RS [(Char, Ordering, Int, String)] String deriving (Show)

data RuleLine = RL String RuleSpec deriving (Show)

type Part a = (a, a, a, a)

rPart :: Char -> Part y -> y
rPart 'x' (x, _, _, _) = x
rPart 'm' (_, m, _, _) = m
rPart 'a' (_, _, a, _) = a
rPart 's' (_, _, _, s) = s
rPart ch _ = error ("Bad component " ++ [ch])

wPart :: Char -> Part y -> y -> Part y
wPart 'x' (_, m, a, s) x = (x, m, a, s)
wPart 'm' (x, _, a, s) m = (x, m, a, s)
wPart 'a' (x, m, _, s) a = (x, m, a, s)
wPart 's' (x, m, a, _) s = (x, m, a, s)
wPart ch _ _ = error ("Bad component " ++ [ch])

readRL :: ReadS RuleLine
readRL = readP_to_S prsr
  where
    prsr = RL <$> many1 (satisfy isAlphaNum) <*> (char '{' *> rsParser <* char '}')
    rsParser =
        (RS [] <$> many1 (satisfy (/= ',')))
            +++ (RS <$> endBy iPrsr (char ',') <*> many1 (satisfy (/= ',')))
    iPrsr =
        (,,,)
            <$> satisfy isAlpha
            <*> ((char '<' $> LT) +++ (char '>' $> GT))
            <*> (read <$> many1 (satisfy isDigit))
            <*> (char ':' *> many1 (satisfy isAlphaNum))

readPart :: ReadS (Part Int)
readPart = readP_to_S prsr
  where
    prsr =
        (,,,)
            <$> (string "{x=" *> (read <$> many1 (satisfy isDigit)))
            <*> (string ",m=" *> (read <$> many1 (satisfy isDigit)))
            <*> (string ",a=" *> (read <$> many1 (satisfy isDigit)))
            <*> (string ",s=" *> (read <$> many1 (satisfy isDigit)))
            <* char '}'

runPartIntOnce :: RuleSpec -> Part Int -> String
runPartIntOnce (RS rules def) part = go rules
  where
    go [] = def
    go ((ch, o, val, ans) : _) | rPart ch part `compare` val == o = ans
    go (_ : rest) = go rest

ruleLineToPair :: RuleLine -> (String, RuleSpec)
ruleLineToPair (RL a b) = (a, b)

runPartIntFull :: M.Map String RuleSpec -> Part Int -> Bool
runPartIntFull rules part = go "in"
  where
    go st =
        let resSt = runPartIntOnce (rules M.! st) part
         in ((resSt == "A") || ((resSt /= "R") && go resSt))

partIntValue :: Part Int -> Int
partIntValue (x, m, a, s) = x + m + a + s

-- range convention in *inclusive* of both ends: (a, b) means a <= x <= b

runPartRangeOnce :: RuleSpec -> Part (Int, Int) -> [(String, Part (Int, Int))]
runPartRangeOnce (RS rules def) part = go part rules
  where
    go p [] = [(def, p)]
    go p rs@((ch, o, val, ans) : rs')
        | fst rd `compare` val == o, snd rd `compare` val == o = [(ans, p)]
        | fst rd `compare` val /= o, snd rd `compare` val /= o = go p rs'
        | otherwise =
            go (wt (fst rd, val - 1)) rs
                ++ go (wt (val + 1, snd rd)) rs
                ++ go (wt (val, val)) rs
      where
        rd = rPart ch p
        wt = wPart ch p

runPartRangeFull :: M.Map String RuleSpec -> Part (Int, Int) -> [Part (Int, Int)]
runPartRangeFull rules part0 = snd <$> go ("in", part0)
  where
    go (flow, part) =
        let once = runPartRangeOnce (rules M.! flow) part
         in filter ((== "A") . fst) once ++ concatMap go (filter ((`notElem` ["R", "A"]) . fst) once)

partRangeValue :: Part (Int, Int) -> Int
partRangeValue (x, m, a, s) =
    let rval (lo, hi) = hi - lo + 1
     in rval x * rval m * rval a * rval s

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc19.in" else head args
    s <- splitOn "\n\n" <$> readFile filename
    let ruleLines = [rl | ln <- lines (head s), (rl, "") <- readRL ln]
    let parts = [pt | ln <- lines (last s), (pt, "") <- readPart ln]
    let ruleDict = M.fromList $ ruleLineToPair <$> ruleLines
    let accepted1 = filter (runPartIntFull ruleDict) parts
    print $ sum $ partIntValue <$> accepted1
    let fullRange = ((1, 4000), (1, 4000), (1, 4000), (1, 4000))
    print $ sum $ partRangeValue <$> runPartRangeFull ruleDict fullRange
