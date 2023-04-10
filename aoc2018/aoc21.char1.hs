import Data.Ord
import Data.Bits
import Data.Maybe
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

-- generate an infinite list of validvalues
-- look for a pattern


main = do
  let allSolutions = emulate [65536, 0, 14906355, 0] 
  -- putStrLn $ show . take 5 $ allSolutions 
  putStrLn $ show $ solve allSolutions
  return ()



-- looks for the first repeat and selects the prior item
solve :: [Int] -> Int
solve = go (-1) IS.empty
  where
    go prev seen (x:xs)
      | IS.member x seen = prev
      | otherwise = go x (IS.insert x seen) xs

-- creates an infinite list of solutions sorted by amount of operations necessary,  the list repeats itself eventually (initial arguments and operations derived from input)
emulate :: [Int] -> [Int]
emulate (b:c:d:e:_)
  | addD      = tmpD:(emulate [tmpB, tmpC, tmpD, tmpE2])
  | otherwise = emulate [tmpB, tmpC, tmpD, tmpE2]
  where
    tmpE   = b .&. 255 
    tmpD   = ((((d + tmpE) .&. 16777215) * 65899) .&. 16777215)
    addD   = 256 > b
    tmpE2  = head aboveB
    aboveB = [ x | x <- [0..], (x + 1) * 256 > b]
    tmpC   = (tmpE2 + 1) *256
    tmpB   = tmpE2

{-

     0	seti 123 0 3
     1	bani 3 456 3
     2	eqri 3 72 3
     3	addr 3 5 5
     4	seti 0 0 5
     5	seti 0 9 3
     6	bori 3 65536 1
     7	seti 14906355 8 3
     8	bani 1 255 4
     9	addr 3 4 3
    10	bani 3 16777215 3
    11	muli 3 65899 3
    12	bani 3 16777215 3
    13	gtir 256 1 4
    14	addr 4 5 5
    15	addi 5 1 5
    16	seti 27 8 5
    17	seti 0 4 4
    18	addi 4 1 2
    19	muli 2 256 2
    20	gtrr 2 1 2
    21	addr 2 5 5
    22	addi 5 1 5
    23	seti 25 1 5
    24	addi 4 1 4
    25	seti 17 2 5
    26	setr 4 9 1
    27	seti 7 0 5
    28	eqrr 3 0 4
    29	addr 4 5 5
    30	seti 5 3 5

  lbl6: bori 3 65536 1
  {-7-} seti 14906355 8 3
  lbl8: bani 1 255 4
  {-9-} addr 3 4 3
 {-10-} bani 3 16777215 3
 {-11-} muli 3 65899 3
 {-12-} bani 3 16777215 3
        if (256 > reg1): goto lbl28
 {-17-} seti 0 4 4
 lbl18: addi 4 1 2
 {-19-} muli 2 256 2
        if (reg2 > reg1): goto lbl26
 {-24-} addi 4 1 4
        goto lbl18
 {-26-} setr 4 9 1
        goto lbl8
 lbl28: if (reg3 != reg1): goto lbl6

-}
