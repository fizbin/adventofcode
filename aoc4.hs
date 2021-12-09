import System.Environment

every :: Int -> [a] -> [a]
every _ [] = []
every n lst = head lst : every n (drop n lst)

won :: [Int] -> [Int] -> Bool
won board called =
    let boardfound = [if x `elem` called then 1 else 0 | x <- board] :: [Int]
        horizwon = any (\idx -> 5 == sum (take 5 $ drop (5*idx) boardfound)) [0..4]
        vertwon = any (\idx -> 5 == sum (every 5 $ drop idx boardfound)) [0..4]
    in horizwon || vertwon

score :: (Num a, Eq a) => [a] -> [a] -> a
score board called =
    let lastcalled = last called
        unmarked = [x | x <- board, x `notElem` called]
    in lastcalled * sum unmarked

makeboards :: [String] -> [[Int]]
makeboards [] = []
makeboards s = map read (take 25 s) : makeboards (drop 25 s)

p1 :: [[Int]] -> [Int] -> Int
p1 boards called =
    head [score b (take idx called) | idx <- [4..length called], b <- boards, won b (take idx called)]

p2 :: [[Int]] -> [Int] -> Int
p2 boards' called = uncurry score $ go 4 boards'
    where
        go :: Int -> [[Int]] -> ([Int], [Int])
        go _ [] = error "empty boards"
        go idx [a] = if won a (take idx called) then (a, take idx called)
                     else go (idx+1) [a]
        go idx boards = let sofar = take idx called
                            remaining = [b | b <- boards, not (won b sofar)]
                        in go (idx+1) remaining

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc4.in" else head args
  strs <- words <$> readFile filename
  let called = read ("[" ++ head strs ++ "]") :: [Int]
  let boards = makeboards (tail strs)
  print $ p1 boards called
  print $ p2 boards called
  