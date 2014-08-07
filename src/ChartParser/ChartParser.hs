module ChartParser where

import Control.Applicative
import Control.Monad.Error
import Data.List
import Data.Maybe

data ChartItem a = Token String | Item a
  deriving Show

data Rule a = Rule { arity :: Int, matcher :: [ChartItem a] -> Maybe (ChartItem a) }

type Grammar a = [Rule a]

newtype Chart a = Chart { edges :: [Edge a] }
  deriving Show

data Edge a = Edge { label :: ChartItem a, after :: Chart a }
  deriving Show

takeChart :: Int -> Chart a -> [([ChartItem a],Chart a)]
takeChart 0 ch = [([],ch)]
takeChart n (Chart es) = do Edge a ch' <- es
                            (as,ch'') <- takeChart (n-1) ch'
                            return (a:as,ch'')

spanningEdges :: Chart a -> [a]
spanningEdges ch = do Edge l ch' <- edges ch
                      guard $ null (edges ch')
                      case l of
                        Token _ -> []
                        Item x  -> [x]

applyRule :: Rule a -> Chart a -> Chart a
applyRule (Rule n f) ch = Chart $ map (uncurry Edge)
                                      (catMaybes [ (\e -> (e,ch')) <$> f (reverse as)
                                                 | (as,ch') <- takeChart n ch ])

applyGrammar :: Grammar a -> Chart a -> Chart a
applyGrammar g ch = Chart (concatMap edges [ applyRule r ch | r <- g ])

-- repeatedly apply a function to a value, return the last non-Nothing value
saturate :: a -> (a -> Maybe a) -> a
saturate x f = case f x of
                 Nothing -> x
                 Just x' -> saturate x' f

-- like `saturate` except builds a list of all the values
saturateList :: a -> (a -> a) -> (a -> Bool) -> [a]
saturateList x f p = takeWhile (not.p) (iterate f x)

saturateList' :: a -> (a -> Maybe a) -> [a]
saturateList' x f = unfoldr (\y -> do y' <- f y ; return (y,y'))
                            x

saturateChart :: Grammar a -> Chart a -> Chart a
saturateChart g ch = Chart $ concatMap edges
                                       (saturateList'
                                          ch
                                          (\ch' -> do guard $ not (null (edges ch'))
                                                      return $ applyGrammar g ch'))
                                       {-(saturateList ch
                                          (applyGrammar g)
                                          (null.edges))-}

data ParseState a = ParseState { unread :: [String] , chart :: Chart a }
  deriving Show

initialParseState :: [String] -> ParseState a
initialParseState ts = ParseState ts (Chart [])

readToken :: Grammar a -> ParseState a -> Maybe (ParseState a)
readToken _ (ParseState []     _)  = Nothing
readToken g (ParseState (t:ts) ch) = Just (ParseState ts (saturateChart g (Chart [ Edge (Token t) ch ])))

saturateM :: Monad m => a -> (a -> m a) -> (a -> Bool) -> m a
saturateM x f p = do x' <- f x
                     if p x'
                     then return x
                     else saturateM x' f p

parse :: Grammar a -> [String] -> [a]
parse g ts = let ps = saturate (initialParseState ts)
                               (readToken g)
             in spanningEdges (chart ps)