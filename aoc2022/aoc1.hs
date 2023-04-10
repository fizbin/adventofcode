import Data.List
import System.Environment

{- TODO: set up a cabal file and have the package split at hand -}
readElves :: String -> [[Int]]
readElves "" = []
readElves "\n" = []
readElves "\n\n" = []
readElves s =
    let (e, s') = readElf s
     in e : readElves s'
  where
    readElf :: String -> ([Int], String)
    readElf ('\n':'\n':s') = ([], s')
    readElf "\n" = ([], "")
    readElf "" = ([], "")
    readElf q =
        case reads q of
            ((qi, s'):_) ->
                let (ri, s'') = readElf s'
                 in (qi : ri, s'')
            [] -> readElf $ tail q

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc1.in"
                else head args
    s <- readFile filename
    print $ maximum $ map sum $ readElves s
    print $ sum $ take 3 $ reverse $ sort $ map sum $ readElves s
