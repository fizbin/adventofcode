module Main where

import System.Environment (getArgs)
import qualified Data.Map as M
--import Debug.Trace

data Rule =
  RuleChar Char
  | RuleRef Int
  | RuleSeq Rule Rule
  | RuleOr Rule Rule
  deriving (Show, Eq)

applyRule :: M.Map Int Rule -> Rule -> String -> [String]
applyRule _ (RuleChar c) (h:s) = [s | c == h]
applyRule _ (RuleChar _) [] = []
applyRule m (RuleRef i) s = applyRule m ((M.!) m i) s
applyRule m (RuleSeq r1 r2) s = concatMap (applyRule m r2) (applyRule m r1 s)
applyRule m (RuleOr r1 r2) s = applyRule m r1 s ++ applyRule m r2 s

readsRule :: ReadS Rule
readsRule s = [(RuleOr r1 r2, s''') |
                (r1, s')   <- ruleSeqRead s,
                ("|", s'') <- lex s',
                (r2, s''') <- readsRule s'']
              ++ ruleSeqRead s
              ++ ruleCharRead s
  where
    ruleSeqRead ss = [(RuleSeq r1 r2, ss'') |
                       (r1, ss')  <- ruleRefRead ss,
                       (r2, ss'') <- ruleSeqRead ss'] ++ ruleRefRead ss
    ruleRefRead ss = [(RuleRef i, ss') | (i, ss') <- reads ss]
    ruleCharRead ss = [(RuleChar ch, ss') | ('"':ch:"\"", ss') <- lex ss]

parseRuleLine :: String -> (Int, Rule)
parseRuleLine s = head $ do
  (rn, s2)   <- reads s
  (":", s3)  <- lex s2
  (rule, s4) <- readsRule s3
  ("", "")   <- lex s4
  pure (rn, rule)

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "aoc19.in" else head args
  s <- readFile filename
  let (rules, _:rest) = break null (lines s)
      ruleMap = M.fromList (map parseRuleLine rules)
  print $ length $ [ passed | passed <- rest,
                     "" `elem` applyRule ruleMap ((M.!) ruleMap 0) passed ]
  let p2rule8  = fst $ head $ readsRule "42 | 42 8"
      p2rule11 = fst $ head $ readsRule "42 31 | 42 11 31"
      part2Map = M.insert 8 p2rule8 $ M.insert 11 p2rule11 ruleMap
  print $ length $ [ passed | passed <- rest,
                     "" `elem` applyRule part2Map ((M.!) part2Map 0) passed ]
  
