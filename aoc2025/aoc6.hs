{-# LANGUAGE LambdaCase #-}
import Data.List (transpose)
import System.Environment (getArgs)
import GHC.List (unsnoc)
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

handlep2 :: [Int] -> [Int] -> [String] -> [Int]
handlep2 ans _ [] = ans
handlep2 ans cur (l:ls) = case trim l of
    "" -> handlep2 ans [] ls
    l' -> case reads l' of
        [] -> error ("Couldn't read " ++ l)
        ((x, rest):_) -> case words rest of
                            [] -> handlep2 ans (x:cur) ls
                            ["*"] -> handlep2 (product (x:cur):ans) [] ls
                            ["+"] -> handlep2 (sum (x:cur):ans) [] ls
                            _ -> error ("Bad leftovers: " ++ rest)
                            

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc6.in"
          (x:_) -> x
  dataLines <- lines <$> readFile filename
  case unsnoc dataLines of
    Nothing -> error "Empty data input"
    Just (numLines, opLine) -> do
        let tnums :: [[Int]] = map (map read . words) numLines
        let nums = transpose tnums
        let ansList = zipWith (\ns -> \case
                            "*" -> product ns
                            "+" -> sum ns
                            w -> error w) nums (words opLine)
        print (sum ansList)
  let part2data = reverse $ transpose dataLines
  print $ sum $ handlep2 [] [] part2data