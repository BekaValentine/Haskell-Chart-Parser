module Example where

import Text.Read

import ChartParser
import Helpers



-- Parse an ambiguous mathematical expression to a number

rule_number :: Rule Int
rule_number = unaryRule $ \x ->
                case x of
                  Token n -> fmap Item (readMaybe n)
                  _       -> Nothing

rule_add :: Rule Int
rule_add = ternaryRule $ \l m r ->
             case (l,m,r) of
               (Item n, Token "+", Item n')
                 -> Just (Item (n + n'))
               _ -> Nothing

rule_sub :: Rule Int
rule_sub = ternaryRule $ \l m r ->
             case (l,m,r) of
               (Item n, Token "-", Item n')
                 -> Just (Item (n - n'))
               _ -> Nothing

exp_grammar = [ rule_number, rule_add, rule_sub ]

ex0 = parse exp_grammar (words "4 - 3")
-- ex0 == [1]

ex1 = parse exp_grammar (words "4 - 3 - 2")
-- ex1 == [-1,3]



-- A CFG example for a fragment of English

data Cat = S | NP | D | N | VP | TV | ITV | PP | P
  deriving (Show,Eq)

eng_grammar :: Grammar (Tree Cat)
eng_grammar = [ S  ==> [ntm NP, ntm VP]
              , NP ==> [ntm D, ntm N]
              , VP ==> [ntm TV, ntm NP]
              , VP ==> [ntm ITV]
              , PP ==> [ntm P, ntm NP]
              , VP ==> [ntm VP, ntm PP]
              , NP ==> [ntm NP, ntm PP]
              , NP ==> [tm "John"]
              , N  ==> [tm "man"]
              , N  ==> [tm "telescope"]
              , D  ==> [tm "a"]
              , D  ==> [tm "the"]
              , TV ==> [tm "saw"]
              , P  ==> [tm "with"]
              ]

sentence = words "John saw the man with a telescope"

ex2 = parse eng_grammar sentence
-- ex contains
--   [S [NP John] [VP [VP [TV saw] [NP [D the] [N man]]] [PP [P with] [NP [D a] [N telescope]]]]]
-- and
--   [S [NP John] [VP [TV saw] [NP [NP [D the] [N man]] [PP [P with] [NP [D a] [N telescope]]]]]]
-- using bracketed string notation for trees (http://ironcreek.net/phpsyntaxtree/)



-- A simple Categorial Grammar parser with / and \ elims only

data Type = S' | NP' | N' | Type :\: Type | Type :/: Type
  deriving (Show,Eq)

data Sign = String ::: Type
  deriving Show

arg_fun :: Rule Sign
arg_fun = binaryRule $ \l r ->
            case (l,r) of
              (Item (x ::: s), Item (f ::: (s' :\: t)))
                | s == s' -> Just . Item $ ("(" ++ x ++ " " ++ f ++ ")") ::: t
              _ -> Nothing

fun_arg :: Rule Sign
fun_arg = binaryRule $ \l r ->
            case (l,r) of
              (Item (f ::: (t :/: s)), Item (x ::: s'))
                | s == s' -> Just . Item $ ("(" ++ f ++ " " ++ x ++ ")") ::: t
              _ -> Nothing

lex_ax :: String -> Type -> Rule Sign
lex_ax s t = unaryRule $ \x ->
             case x of
               Token s' | s' == s -> Just . Item $ s ::: t
               _ -> Nothing

john_lex      = lex_ax "John"      NP'
man_lex       = lex_ax "man"       N'
telescope_lex = lex_ax "telescope" N'
the_lex       = lex_ax "the"       (NP' :/: N')
a_lex         = lex_ax "a"         (NP' :/: N')
saw_lex       = lex_ax "saw"       ((NP' :\: S') :/: NP')
with_adv_lex  = lex_ax "with_adv"  (((NP' :\: S') :\: (NP' :\: S')) :/: NP')
with_adn_lex  = lex_ax "with_adn"  ((N' :\: N') :/: NP')

cg_rules = [ arg_fun, fun_arg
           , john_lex, man_lex, telescope_lex, saw_lex
           , the_lex, a_lex, with_adv_lex, with_adn_lex
           ]

ex4 = parse cg_rules sentence
-- ex4 contains
--   "(John ((saw (the man)) (with_adv (a telescope))))" ::: S'
-- and
--   "(John (saw (the (man (with_adn (a telescope))))))" ::: S'