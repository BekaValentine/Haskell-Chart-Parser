module Helpers where

import Control.Monad.Error

import ChartParser



-- General helpers

unaryRule :: (ChartItem a -> Maybe (ChartItem a)) -> Rule a
unaryRule f = Rule 1 $ \xs ->
                case xs of
                  [x] -> f x
                  _   -> Nothing

binaryRule :: (ChartItem a -> ChartItem a -> Maybe (ChartItem a)) -> Rule a
binaryRule f = Rule 2 $ \xs ->
                 case xs of
                   [l,r] -> f l r
                   _     -> Nothing

ternaryRule :: (ChartItem a -> ChartItem a -> ChartItem a -> Maybe (ChartItem a)) -> Rule a
ternaryRule f = Rule 3 $ \xs ->
                 case xs of
                   [l,m,r] -> f l m r
                   _       -> Nothing



-- Helpers for CFGs

data Tree c = Word String | Phrase c [Tree c]

instance Show c => Show (Tree c) where
  show (Word s)      = s
  show (Phrase c ds) = "[" ++ show c ++ " " ++ unwords (map show ds) ++ "]"

cat :: Tree c -> Maybe c
cat (Word _)     = Nothing
cat (Phrase c _) = Just c

data CFGItem c = Terminal String | Nonterminal c

tm = Terminal
ntm = Nonterminal

(==>) :: Eq c => c -> [CFGItem c] -> Rule (Tree c)
m ==> ds = Rule (length ds) $ \is ->
             do ts <- matchCats ds is
                return $ Item (Phrase m ts)
  where matchCats :: Eq c => [CFGItem c] -> [ChartItem (Tree c)] -> Maybe [Tree c]
        matchCats [] [] = Just []
        matchCats (Terminal s:cs) (Token s':is)
          | s == s' = do ts <- matchCats cs is
                         return $ Word s:ts
        matchCats (Nonterminal c:cs) (Item t:is)
          | Just c == cat t = do ts <- matchCats cs is
                                 return $ t:ts
        matchCats _ _ = Nothing