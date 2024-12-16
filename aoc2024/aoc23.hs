import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc23.in" else head args
  mydata <- lines <$> readFile filename
  print $ length mydata
