import Control.Monad (forM)
import Data.Char (isSpace)
import Data.List (foldl', isPrefixOf)
import System.Environment (getArgs)

type Hand = (Int, Int, Int)

readsConst :: String -> String -> [(String, String)]
readsConst needle haystack = [(needle, drop (length needle) haystack) | needle `isPrefixOf` haystack]

readsGame :: String -> [((Int, [Hand]), String)]
readsGame r =
    [ ((i, hs), r3) | ("Game", istr) <- [break isSpace r], (i, r2) <- reads istr, (hs, r3) <- readHands $ dropWhile (`elem` ": ") r2
    ]
  where
    readHands [] = [([], [])]
    readHands (' ' : s) = readHands s
    readHands (';' : s) = readHands s
    readHands s = [(h : hs, s2) | (h, s1) <- readsHand s, (hs, s2) <- readHands s1]
    readsHand :: String -> [(Hand, String)]
    readsHand [] = [((0, 0, 0), "")]
    readsHand (' ' : s) = readsHand s
    readsHand s@(';' : _) = [((0, 0, 0), s)]
    readsHand s = do
        (i, rs) <- reads (map (\c -> if c == ',' then ' ' else c) s)
        [((rd + i, gr, bl), s3) | (_, s2) <- readsConst " red" rs, ((rd, gr, bl), s3) <- readsHand s2]
            ++ [((rd, gr + i, bl), s3) | (_, s2) <- readsConst " green" rs, ((rd, gr, bl), s3) <- readsHand s2]
            ++ [((rd, gr, bl + i), s3) | (_, s2) <- readsConst " blue" rs, ((rd, gr, bl), s3) <- readsHand s2]

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc2.in"
                else head args
    s <- lines <$> readFile filename
    games <- forM s $ \line -> case readsGame line of
        ((game, "") : _) -> pure game
        _ -> ioError (userError $ "bad parse on line: " ++ line)
    let maxHand (r1, g1, b1) (r2, g2, b2) = (max r1 r2, max g1 g2, max b1 b2)
    let acceptable (_, hands) =
            let (maxr, maxg, maxb) = foldl' maxHand (0, 0, 0) hands
             in (maxr <= 12) && (maxg <= 13) && (maxb <= 14)
    print $ sum $ map fst $ filter acceptable games
    print $ sum $ flip map games $ \(_, hands) ->
        let (maxr, maxg, maxb) = foldl' maxHand (0, 0, 0) hands in maxr * maxg * maxb