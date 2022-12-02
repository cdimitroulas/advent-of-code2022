module Lib.Common (safeTake, split, splitList, solve) where

import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

-- TODO: Come up with a generic "solver" function which reads a file, parses the input and then
-- calculates the solution.

type Filepath = Text

-- parseFileLines :: (String -> Maybe a) -> Filepath -> IO (Maybe [a])
-- parseFileLines parse filepath = T.mapM parse . T.lines <$> readFile filepath

solve :: FilePath -- path to the input data
    -> ([Text] -> Maybe a) -- Parser
    -> (a -> b) -- Solver
    -> IO (Maybe b) -- Solution
solve filepath parser solver = fmap solver . parser . T.lines <$> TIO.readFile filepath

safeTake :: Int -> [a] -> Maybe [a]
safeTake 0 _           = Just []
safeTake amount (x:xs) = (:) <$> Just x <*> safeTake (amount -1) xs
safeTake _ []          = Nothing

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

