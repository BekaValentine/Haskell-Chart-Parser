module ChartParser where

import Control.Applicative
import Control.Monad.Error
import Data.Maybe

data Rule a = Rule { arity :: Int, matcher :: [a] -> Maybe a }

type Grammar a = [Rule a]

newtype Chart a = Chart { edges :: [Edge a] }
  deriving (Show,Eq)

data Edge a = Edge { label :: a, after :: Chart a }
  deriving (Show,Eq)

takeChart :: Int -> Chart a -> [([a],Chart a)]
takeChart 0 ch = [([],ch)]
takeChart n (Chart es) = do Edge a ch' <- es
                            (as,ch'') <- takeChart (n-1) ch'
                            return (a:as,ch'')

spanningEdges :: Chart a -> [Edge a]
spanningEdges ch = [ e | e <- edges ch, null (edges (after e)) ]

applyRule :: Rule a -> Chart a -> Chart a
applyRule (Rule n f) ch = Chart $ map (uncurry Edge)
                                      (catMaybes [ (\e -> (e,ch')) <$> f (reverse as)
                                                 | (as,ch') <- takeChart n ch ])

applyGrammar :: Grammar a -> Chart a -> Chart a
applyGrammar g ch = Chart (concatMap edges [ applyRule r ch | r <- g ])

saturate :: a -> (a -> Maybe a) -> a
saturate x f = case f x of
                 Nothing -> x
                 Just x' -> saturate x' f

saturateList :: a -> (a -> a) -> (a -> Bool) -> [a]
saturateList x f p = takeWhile (not.p) (iterate f x)

saturateChart :: Grammar a -> Chart a -> Chart a
saturateChart g ch = Chart $ concatMap edges
                                       (saturateList ch
                                          (applyGrammar g)
                                          (null.edges))

data ParseState a = ParseState { unread :: [[a]] , chart :: Chart a }
  deriving (Show,Eq)

readWord :: Grammar a -> ParseState a -> Maybe (ParseState a)
readWord _ (ParseState []     _)    = Nothing
readWord g (ParseState (ws:wss) ch) = Just (ParseState wss (saturateChart g (Chart [ Edge w ch | w <- ws ])))

saturateM :: Monad m => a -> (a -> m a) -> (a -> Bool) -> m a
saturateM x f p = do x' <- f x
                     if p x'
                     then return x
                     else saturateM x' f p

parse :: Grammar a -> [[a]] -> Either String [a]
parse g wss = let ps = saturate (ParseState wss (Chart []))
                                (readWord g)
                  spans = map label (spanningEdges (chart ps))
              in if null spans
                 then throwError "No parse."
                 else return spans