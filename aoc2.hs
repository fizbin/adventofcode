import System.Environment

-- The huge numbers are things that'll make the total stand out as "something went wrong"
score1 s = scoreMine (tail $ tail s) + scoreWLD s
  where
    scoreMine "X" = 1
    scoreMine "Y" = 2
    scoreMine "Z" = 3
    scoreMine _ = 9999999999999999999999
    scoreWLD "A X" = 3
    scoreWLD "A Y" = 6
    scoreWLD "A Z" = 0
    scoreWLD "B X" = 0
    scoreWLD "B Y" = 3
    scoreWLD "B Z" = 6
    scoreWLD "C X" = 6
    scoreWLD "C Y" = 0
    scoreWLD "C Z" = 3
    scoreWLD _ = 88888888888888888888

score2 s = scoreMine (tail $ tail s) + scoreWLD s
  where
    scoreMine "X" = 0
    scoreMine "Y" = 3
    scoreMine "Z" = 6
    scoreMine _ = 9999999999999999999999
    scoreWLD "A X" = 3
    scoreWLD "A Y" = 1
    scoreWLD "A Z" = 2
    scoreWLD "B X" = 1
    scoreWLD "B Y" = 2
    scoreWLD "B Z" = 3
    scoreWLD "C X" = 2
    scoreWLD "C Y" = 3
    scoreWLD "C Z" = 1
    scoreWLD _ = 88888888888888888888

main :: IO ()
main = do
    args <- getArgs
    let filename =
            if null args
                then "aoc2.in"
                else head args
    s <- lines <$> readFile filename
    print $ sum $ map score1 s
    print $ sum $ map score2 s
