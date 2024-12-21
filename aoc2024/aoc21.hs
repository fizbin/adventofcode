import Data.Map (Map)
import Data.Map qualified as M
import System.Environment (getArgs)

dirpad :: Char -> (Int, Int)
dirpad '^' = (0, 1)
dirpad 'A' = (0, 2)
dirpad '<' = (1, 0)
dirpad 'v' = (1, 1)
dirpad '>' = (1, 2)
dirpad sym = error ("Unknown dirpad symbol " ++ [sym])

numpad :: Char -> (Int, Int)
numpad '7' = (0, 0)
numpad '8' = (0, 1)
numpad '9' = (0, 2)
numpad '4' = (1, 0)
numpad '5' = (1, 1)
numpad '6' = (1, 2)
numpad '1' = (2, 0)
numpad '2' = (2, 1)
numpad '3' = (2, 2)
-- numpad ' ' =
numpad '0' = (3, 1)
numpad 'A' = (3, 2)
numpad sym = error ("Unknown numpad symbol " ++ [sym])

dirpad1 :: [Map (Char, Char) Int] -> Char -> Char -> (Int, [Map (Char, Char) Int])
dirpad1 [] _ _ = error "Calling error"
dirpad1 (ctop : crest) von zu = case M.lookup (von, zu) ctop of
  Just ans -> (ans, ctop : crest)
  Nothing ->
    let vonloc = dirpad von
        zuloc = dirpad zu
        upstr = replicate (max 0 $ fst vonloc - fst zuloc) '^'
        dnstr = replicate (max 0 $ fst zuloc - fst vonloc) 'v'
        lfstr = replicate (max 0 $ snd vonloc - snd zuloc) '<'
        rtstr = replicate (max 0 $ snd zuloc - snd vonloc) '>'
        rowfirst = upstr ++ dnstr ++ lfstr ++ rtstr ++ "A"
        colfirst = lfstr ++ rtstr ++ upstr ++ dnstr ++ "A"
        (ans, ncrest) = case (vonloc, zuloc) of
          ((0, _), (_, 0)) -> dirpadseq crest rowfirst
          ((_, 0), (0, _)) -> dirpadseq crest colfirst
          _ ->
            let (a, crest') = dirpadseq crest rowfirst
                (b, crest'') = dirpadseq crest' colfirst
             in (min a b, crest'')
     in (ans, M.insert (von, zu) ans ctop : ncrest)

dirpadseq :: [Map (Char, Char) Int] -> [Char] -> (Int, [Map (Char, Char) Int])
dirpadseq [] s = (length s, [])
dirpadseq cs s = go 0 cs ('A' : s)
  where
    go acc caches (pspot : rs@(spot : _)) =
      let (subans, caches') = dirpad1 caches pspot spot
       in go (acc + subans) caches' rs
    go acc caches _ = (acc, caches)

numpad1 :: [Map (Char, Char) Int] -> Char -> Char -> (Int, [Map (Char, Char) Int])
numpad1 [] _ _ = error "Calling error"
numpad1 caches von zu =
  let vonloc = numpad von
      zuloc = numpad zu
      upstr = replicate (max 0 $ fst vonloc - fst zuloc) '^'
      dnstr = replicate (max 0 $ fst zuloc - fst vonloc) 'v'
      lfstr = replicate (max 0 $ snd vonloc - snd zuloc) '<'
      rtstr = replicate (max 0 $ snd zuloc - snd vonloc) '>'
      rowfirst = upstr ++ dnstr ++ lfstr ++ rtstr ++ "A"
      colfirst = lfstr ++ rtstr ++ upstr ++ dnstr ++ "A"
      (ans, ncaches) = case (vonloc, zuloc) of
        ((3, _), (_, 0)) -> dirpadseq caches rowfirst
        ((_, 0), (3, _)) -> dirpadseq caches colfirst
        _ ->
          let (a, caches') = dirpadseq caches rowfirst
              (b, caches'') = dirpadseq caches' colfirst
           in (min a b, caches'')
   in (ans, ncaches)

numpadseq :: [Map (Char, Char) Int] -> [Char] -> (Int, [Map (Char, Char) Int])
numpadseq [] s = (length s, [])
numpadseq cs s = go 0 cs ('A' : s)
  where
    go acc caches (pspot : rs@(spot : _)) =
      let (subans, caches') = numpad1 caches pspot spot
       in go (acc + subans) caches' rs
    go acc caches _ = (acc, caches)

handleCode :: Int -> String -> Int
handleCode nIntermediate code =
  let (codeval, _) = head $ reads code
      (presses, _) = numpadseq (replicate nIntermediate M.empty) code
   in codeval * presses

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc21.in" else head args
  mydata <- lines <$> readFile filename
  putStr "Part 1: "
  print $ sum $ handleCode 2 <$> mydata
  putStr "Part 2: "
  print $ sum $ handleCode 25 <$> mydata
