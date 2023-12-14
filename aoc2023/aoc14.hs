import Data.ByteString qualified as B
import Data.List (transpose)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import System.Environment (getArgs)

westLoad :: [[Char]] -> Int
westLoad = sum . map westLoad1
  where
    westLoad1 s = let n = length s in sum [n - p | (p, ch) <- zip [0 ..] s, ch == 'O']

northLoad :: [[Char]] -> Int
northLoad = westLoad . transpose

rollWest :: [[Char]] -> [[Char]]
rollWest = map (rollWest1 0 0)
  where
    rollWest1 nOs nDots [] = replicate nOs 'O' ++ replicate nDots '.'
    rollWest1 nOs nDots ('O' : s) = rollWest1 (nOs + 1) nDots s
    rollWest1 nOs nDots ('.' : s) = rollWest1 nOs (nDots + 1) s
    rollWest1 nOs nDots ('#' : s) = rollWest1 nOs nDots [] ++ ('#' : rollWest1 0 0 s)
    rollWest1 _ _ s = error $ "rollWest1 " ++ show s

rollNorth :: [[Char]] -> [[Char]]
rollNorth = transpose . rollWest . transpose
rollEast :: [[Char]] -> [[Char]]
rollEast = map reverse . rollWest . map reverse
rollSouth :: [[Char]] -> [[Char]]
rollSouth = reverse . rollNorth . reverse

gridSig :: [[Char]] -> B.ByteString
gridSig = encodeUtf8 . T.pack . concat

repeatHuge :: (Ord b) => Int -> a -> (a -> a) -> (a -> b) -> a
repeatHuge limit start rep sigMaker =
    go 0 start M.empty
  where
    go n a _ | n == limit = a
    go n a m =
        let mySig = sigMaker a
         in case M.lookup mySig m of
                Just prevN -> iterate rep a !! ((limit - n) `mod` (n - prevN))
                Nothing -> go (n + 1) (rep a) (M.insert mySig n m)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "aoc14.in" else head args
    grid <- lines <$> readFile filename
    print $ northLoad $ rollNorth grid
    let spinCycle = rollEast . rollSouth . rollWest . rollNorth
    let fgrid = repeatHuge 1000000000 grid spinCycle gridSig
    print $ northLoad fgrid
