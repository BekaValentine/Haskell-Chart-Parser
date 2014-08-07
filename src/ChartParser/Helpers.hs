module Helpers where

import Control.Monad.Error

import ChartParser



-- General helpers

unaryRule :: (a -> Maybe a) -> Rule a
unaryRule f = Rule 1 $ \xs ->
                case xs of
                  [x] -> f x
                  _   -> Nothing

binaryRule :: (a -> a -> Maybe a) -> Rule a
binaryRule f = Rule 2 $ \xs ->
                 case xs of
                   [l,r] -> f l r
                   _     -> Nothing

ternaryRule :: (a -> a -> a -> Maybe a) -> Rule a
ternaryRule f = Rule 2 $ \xs ->
                 case xs of
                   [l,m,r] -> f l m r
                   _       -> Nothing

type Lexicon a = String -> [a]

lexer :: Lexicon a -> [String] -> [[a]]
lexer l = map l



-- Helpers for CFGs

data Tree c = Word c String | Phrase c [Tree c]

instance Show c => Show (Tree c) where
  show (Word c s)    = "[" ++ show c ++ " " ++ s ++ "]"
  show (Phrase c ds) = "[" ++ show c ++ " " ++ unwords (map show ds) ++ "]"

cat :: Tree c -> c
cat (Word c _)   = c
cat (Phrase c _) = c

(==>) :: Eq c => c -> [c] -> Rule (Tree c)
m ==> ds = Rule (length ds) $ \ts ->
             do guard $ ds == map cat ts
                return $ Phrase m ts