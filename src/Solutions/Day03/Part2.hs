module Solutions.Day03.Part2 (solution_2) where

import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Lib.Common             (solve, uniq)
import           Solutions.Day03.Common (BackpackItem, getItemPriority)

-- Splits a list into a list of 3-length tuples. Errors if the list isn't divisible by 3
makeGroups :: [a] -> [(a, a, a)]
makeGroups (x1:x2:x3:xs) = [(x1, x2, x3)] <> makeGroups xs
makeGroups []            = []
makeGroups _             = error "List was not divisible by 3"

findGroupCommonItem :: (Text, Text, Text) -> BackpackItem
findGroupCommonItem (bp1, bp2, bp3) =
  findCommonElem3 (T.unpack bp1) (T.unpack bp2) (T.unpack bp3)

-- Creates a map counting how many times each backpack item type appears across three
-- backpacks, only counting each type once within a specific backpack.
findCommonElem3 :: Eq a => Ord a => Show a => [a] -> [a] -> [a] -> a
findCommonElem3 listA listB = go
  where
    initialCounts = Map.fromList (map (, 1) listA)
    -- The list of common backpack items between list A and B
    commonABElems = Map.filter (== 2) (countUniqueItems initialCounts listB)
    -- Go through list C and find the backpack type which exists in the common backpack types
    -- from list A and B
    go (x:xs) = case Map.lookup x commonABElems of
                  Just _  -> x
                  Nothing -> go xs
    go [] = error "Could not find backpack item type which was in all three backpacks"

countUniqueItems :: Ord a => Eq a => Map a Int -> [a] -> Map a Int
countUniqueItems m list = foldl adjustCount m (uniq list)
  where
    addOrInit Nothing  = Just 1
    addOrInit (Just x) = Just (1 + x)
    adjustCount = flip $ Map.alter addOrInit

solution_2 :: IO ()
solution_2 = do
  result <- solve "data/day03.txt" (Just . makeGroups) solver
  print result
  where
    solver :: [(Text, Text, Text)] -> Int
    solver = sum . map (getItemPriority . findGroupCommonItem)

