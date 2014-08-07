{-# OPTIONS -Wall #-}

module Example where

import ChartParser
import Helpers

data Cat = S | NP | D | N | VP | TV | ITV | PP | P
  deriving (Show,Eq)

lexicon :: Lexicon (Tree Cat)
lexicon "the"    = [Word D "the"]
lexicon "a"      = [Word D "a"]
lexicon "dog"    = [Word N "dog"]
lexicon "man"    = [Word N "man"]
lexicon "street" = [Word N "street"]
lexicon "bit"    = [Word TV "bit"]
lexicon "on"     = [Word P "on"]
lexicon _        = []

grammar :: Grammar (Tree Cat)
grammar = [ S  ==> [NP,VP]
          , NP ==> [D,N]
          , VP ==> [TV,NP]
          , VP ==> [ITV]
          , PP ==> [P,NP]
          , VP ==> [VP,PP]
          , NP ==> [NP,PP]
          ]

main :: IO ()
main = do print input
          let p = parse grammar input
          print p
  where input :: [[Tree Cat]]
        input = lexer lexicon (words "the dog bit a man on the street")