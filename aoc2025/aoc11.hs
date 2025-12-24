import Data.Map.Lazy qualified as M
import Data.Map.Strict qualified as MS
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of
          [] -> "aoc11.in"
          (x:_) -> x
  dataLines <- lines <$> readFile filename
  let connections =
        MS.fromList $ do
          line <- dataLines
          case break (== ':') line of
            (prenom, ':':rest) -> pure (prenom, words rest)
            _ -> []
  print $ part1 connections
  print $ part2 connections

part1 :: M.Map String [String] -> Int
part1 connections = M.findWithDefault 0 "you" comboMap
  where
    comboMap =
      MS.insert "out" 1
        $ M.map
            (\dests -> sum $ (flip $ M.findWithDefault 0) comboMap <$> dests)
            connections

part2 :: M.Map String [String] -> Int
part2 connections =
  (\(_, _, _, x) -> x) $ M.findWithDefault four0 "svr" comboMap
  where
    four0 = (0, 0, 0, 0) :: (Int, Int, Int, Int)
    comboMap =
      MS.insert "out" (1, 0, 0, 0) $ M.mapWithKey comboMaker connections
    foursum :: [(Int, Int, Int, Int)] -> (Int, Int, Int, Int)
    foursum =
      foldr
        (\(a1, a2, a3, a4) (b1, b2, b3, b4) ->
           (a1 + b1, a2 + b2, a3 + b3, a4 + b4))
        (0, 0, 0, 0)
    comboMaker "fft" conns =
      let (a, b, c, d) =
            foursum $ (flip $ M.findWithDefault four0) comboMap <$> conns
       in (0, a + b, 0, c + d)
    comboMaker "dac" conns =
      let (a, b, c, d) =
            foursum $ (flip $ M.findWithDefault four0) comboMap <$> conns
       in (0, 0, a + c, b + d)
    comboMaker _ conns =
      foursum $ (flip $ M.findWithDefault four0) comboMap <$> conns
