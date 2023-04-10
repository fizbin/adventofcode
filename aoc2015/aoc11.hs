{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Maybe

data PairState = NoPair | OnePair Char | BothPair deriving (Show, Eq)

data PullState = PS
  { soFarRev :: [Char],
    toGo :: [Char],
    hasStraight :: Bool,
    pairState :: PairState
  }

incWord' :: String -> Maybe String
incWord' = go
  where
    go [] = Nothing
    go ('z' : s) = (:) 'z' <$> go s
    go (c : s) = case go s of
      Nothing ->
        let nc = succ c
            nc' = if nc `elem` "ilo" then succ nc else nc
         in Just $ nc' : replicate (length s) 'a'
      Just t -> Just (c : t)

nextPass :: String -> String
nextPass pw0 = fromJust $ go $ PS {soFarRev = [], toGo = fromJust (incWord' pw0), hasStraight = False, pairState = NoPair}
  where
    go PS {soFarRev = soFarRev@(a : b : c : _), ..} | not hasStraight && a == succ b && b == succ c = go PS {hasStraight = True, ..}
    go PS {soFarRev = soFarRev@(a : b : _), ..} | a == b && pairState /= BothPair && pairState /= OnePair a = case pairState of
      NoPair -> go PS {pairState = OnePair a, ..}
      OnePair _ -> go PS {pairState = BothPair, ..}
      BothPair -> error "Can't happen"
    go PS {toGo = [], ..} | hasStraight && pairState == BothPair = Just $ reverse soFarRev
    go PS {toGo = [], ..} = Nothing
    go PS {toGo = (a : as), ..} =
      let nextPS = PS {toGo = as, soFarRev = a : soFarRev, ..}
       in case go nextPS of
            Nothing -> incWord' [a] >>= \x -> go PS {toGo = x ++ replicate (length as) 'a', ..}
            x -> x

main :: IO ()
main = do
  let x = nextPass "hxbxwxba"
  print x
  print $ nextPass x