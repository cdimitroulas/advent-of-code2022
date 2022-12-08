module Lib.Common (
  safeTake,
  safeHead,
  safeLast,
  split,
  splitList,
  solve,
  uniq,
  parseMaybe,
  traverseBoth,
  sequenceBoth,
  filterMaybe,
  isUniqueList
) where

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.List            (nub)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

-- TODO: Come up with a generic "solver" function which reads a file, parses the input and then
-- calculates the solution.

type Filepath = Text

-- parseFileLines :: (String -> Maybe a) -> Filepath -> IO (Maybe [a])
-- parseFileLines parse filepath = T.mapM parse . T.lines <$> readFile filepath

solve :: Monad m => FilePath -- path to the input data
    -> ([Text] -> m a) -- Parser
    -> (a -> b) -- Solver
    -> IO (m b) -- Solution
solve filepath parser solver = fmap solver . parser . filter (/= "") . T.lines <$> TIO.readFile filepath

safeTake :: Int -> [a] -> Maybe [a]
safeTake 0 _           = Just []
safeTake amount (x:xs) = (:) <$> Just x <*> safeTake (amount -1) xs
safeTake _ []          = Nothing

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = Just $ last xs

-- Repeatedly splits a list by the provided separator and collects the results
splitList :: Eq a => a -> [a] -> [[a]]
splitList _ [] = []
splitList sep list = h : splitList sep t
  where
    (h, t) = split (== sep) list

split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left, right)
  where
    (left, right') = break f s
    right = if null right' then [] else tail right'


uniq :: Ord a => Eq a => [a] -> [a]
uniq = map fst . Map.toList . Map.fromList . map (, Nothing)

parseMaybe :: Parser a -> Text -> Maybe a
parseMaybe parser = P.maybeResult . P.parse parser

traverseBoth :: Applicative f => (a -> f b) -> (a, a) -> f (b, b)
traverseBoth f (x, y) = (,) <$> f x <*> f y

sequenceBoth :: Applicative f => (f a, f b) -> f (a, b)
sequenceBoth (x, y) = (,) <$> x <*> y

filterMaybe :: [Maybe a] -> [a]
filterMaybe = foldl (\list el -> case el of
                           (Just x) -> list <> [x]
                           Nothing  -> list) []

isUniqueList :: Eq a => [a] -> Bool
isUniqueList list = nub list == list

