{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Arrow (Arrow (second))
import Data.Char (isLetter, isSpace)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

-- import Debug.Trace (trace)

data Expr a
  = -- | value
    Ev a
  | -- | plus
    Ep (Expr a) (Expr a)
  | -- | minus
    Em (Expr a) (Expr a)
  | -- | times
    Et (Expr a) (Expr a)
  | -- | divide
    Ed (Expr a) (Expr a)
  | -- | part 2 only
    Evar
  deriving (Eq, Functor)

evalExpr :: (Num a, Fractional a) => a -> Expr a -> a
evalExpr _ (Ev a) = a
evalExpr v (Ep a b) = evalExpr v a + evalExpr v b
evalExpr v (Em a b) = evalExpr v a - evalExpr v b
evalExpr v (Et a b) = evalExpr v a * evalExpr v b
evalExpr v (Ed a b) = evalExpr v a / evalExpr v b
evalExpr v Evar = v

simplify :: (Integral a, Num a) => Expr a -> Expr a
simplify = \case
  Ep a b -> trySimp Ep (+) a b
  Em a b -> trySimp Em (-) a b
  Et a b -> trySimp Et (*) a b
  Ed a b -> tryEdSimp a b
  e -> e
 where
  trySimp ctor fn a b = case (simplify a, simplify b) of
    (Ev a', Ev b') -> Ev (fn a' b')
    (a', b') -> ctor a' b'
  tryEdSimp a b = case (simplify a, simplify b) of
    (Ev a', Ev b') | a' `mod` b' == 0 -> Ev (a' `div` b')
    (a', b') -> Ed a' b'

instance Show a => Show (Expr a) where
  showsPrec lvl e = case e of
    (Ev v) -> showsPrec lvl v
    (Ep a b) -> thing 6 " + " a b
    (Em a b) -> thing 6 " - " a b
    (Et a b) -> thing 7 " * " a b
    (Ed a b) -> thing 7 " / " a b
    Evar -> ("x" ++)
   where
    thing lim op a b
      | lvl <= lim = showsPrec lim a . (op ++) . showsPrec lim b
      | otherwise = ("(" ++) . showsPrec lim a . (op ++) . showsPrec lim b . (")" ++)

parseExpr :: Read a => (String -> Expr a) -> String -> Expr a
parseExpr recurse line = case parses of
  [] -> error $ "No parse for " ++ show line
  (v, _) : _ -> v
 where
  parses =
    [(Ev v, s) | (v, s) <- reads line]
      ++ [ (ctor (recurse a) (recurse b), s'')
         | (a, s) <- readsMref line
         , (ctor, s') <- readsOp s
         , (b, s'') <- readsMref s'
         ]
  readsMref s =
    let (var, s') = span isLetter $ dropWhile isSpace s
     in [(var, dropWhile isSpace s') | not (null var)]
  readsOp ('+' : s) = [(Ep, dropWhile isSpace s)]
  readsOp ('/' : s) = [(Ed, dropWhile isSpace s)]
  readsOp ('-' : s) = [(Em, dropWhile isSpace s)]
  readsOp ('*' : s) = [(Et, dropWhile isSpace s)]
  readsOp _ = []

bisectInt :: (Ord a, Num a) => (Int -> a) -> Maybe Int
bisectInt f =
  let (a, b) = findInitial 0 1
   in {- trace "bisecting..." $ -} go a b
 where
  findInitial a b
    | fa > fb = findInitial b a
    | fa <= 0 && fb >= 0 = (a, b)
    | fa > 0 = findInitial (2 * a - b) b
    | otherwise = findInitial a (2 * b - a)
   where
    fa = f a; fb = f b
  go a b
    | fa == 0 = Just a
    | fb == 0 = Just b
    | b == a + 1 || a == b + 1 = Nothing
    | f c <= 0 = go c b
    | otherwise = go a c
   where
    fa = f a
    fb = f b
    c = (a + b) `div` 2

secantInt :: (Int -> Rational) -> Maybe Int
secantInt f =
  let (a, b) = findInitial 0 100 :: (Int, Int)
   in {- trace ("secanting..." ++ show (a, b)) $ -} go a b
 where
  findInitial a b
    | fa > fb = findInitial b a
    | fa /= fb = (a, b)
    | otherwise = findInitial a (2 * b - a)
   where
    fa = f a; fb = f b
  go a b
    | fa == 0 = Just a
    | fb == 0 = Just b
    | a == b || b == a + 1 || a == b + 1 = Nothing
    | otherwise = go b c
   where
    fa = f a
    fb = f b
    c = b - round (fb * (fromIntegral (b - a) / (fb - fa)))

reverseToVar :: (Num a, Integral a) => Expr a -> a -> Maybe a
reverseToVar Evar n = Just n
reverseToVar (Ep (Ev a) b) n = reverseToVar b (n - a)
reverseToVar (Ep b (Ev a)) n = reverseToVar b (n - a)
reverseToVar (Em (Ev a) b) n = reverseToVar b (a - n)
reverseToVar (Em b (Ev a)) n = reverseToVar b (n + a)
reverseToVar (Et (Ev a) b) n | n `mod` a == 0 = reverseToVar b (n `div` a)
reverseToVar (Et b (Ev a)) n | n `mod` a == 0 = reverseToVar b (n `div` a)
reverseToVar (Ed (Ev a) b) n | a `mod` n == 0 = reverseToVar b (a `div` n)
reverseToVar (Ed b (Ev a)) n = reverseToVar b (n * a)
reverseToVar _ _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  let (method, args') = case args of
        ("-bisect" : a) -> (0, a) :: (Int, [String])
        ("-secant" : a) -> (1, a)
        ("-rev" : a) -> (2, a)
        (('-' : unk) : _) -> error $ "Unknown option " ++ show ('-' : unk)
        a -> (0, a)
  let filename =
        if null args'
          then "aoc21.in"
          else head args'
  s <- lines <$> readFile filename
  let monkStrs = M.fromList $ map (second (dropWhile isSpace . tail) . span (/= ':')) s

  -- part 1
  let findMonk m = fromMaybe (error $ "Unknown monkey " ++ m) (M.lookup m monkStrs)
  let pFunc = parseExpr pFunc . findMonk
  let part1 = evalExpr 0 $ toRational <$> (pFunc "root" :: Expr Integer)
  print part1

  -- part 2
  -- Replace the op on "root" with "-" and then solve for root == 0
  let findMonk2 m =
        if m == "root"
          then map (\c -> if c `elem` "+/*" then '-' else c) (findMonk m)
          else findMonk m
  -- replace the parse of whatever was there for "humn" with an Evar
  let pFunc2 m =
        if m == "humn"
          then Evar
          else parseExpr pFunc2 $ findMonk2 m
  let expr = simplify $ pFunc2 "root" :: Expr Integer
  case method of
    0 -> print $ bisectInt (\x -> evalExpr (toRational x) (toRational <$> expr))
    1 -> print $ secantInt (\x -> evalExpr (toRational x) (toRational <$> expr))
    2 -> print $ reverseToVar expr 0
    _ -> error "Internal error"