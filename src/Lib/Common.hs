module Lib.Common (
  safeTake,
  safeHead,
  safeLast,
  safeTail,
  split,
  splitList,
  solve,
  uniq,
  parseMaybe,
  traverseBoth,
  sequenceBoth,
  filterMaybe,
  isUniqueList,
  mapWithIndex,
  setAt,
  (!!?)
) where

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.List            (nub)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

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

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

-- safe version of !! stolen from Relude
infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing

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

-- alias for nub because I'll never remember that function
uniq :: Eq a => [a] -> [a]
uniq = nub

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

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = run 0
  where
    run _ []         = []
    run index (x:xs) = f index x : run (index + 1) xs

setAt :: (a -> a) -> Int -> [a] -> [a]
setAt f pos = mapWithIndex (\index -> if index == pos then f else id)
