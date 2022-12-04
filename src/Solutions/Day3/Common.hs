module Solutions.Day3.Common (
  BackpackItem, Backpack
    , parseBackpack, getItemPriority, findCommonElem) where

import           Data.List (elemIndex)
import           Data.Text (Text)
import qualified Data.Text as T

type BackpackItem = Char

-- A backpack has items split into two compartments
type Backpack = ([BackpackItem], [BackpackItem])

parseBackpack :: Text -> Backpack
parseBackpack items = splitAt (T.length items `div` 2) $ T.unpack items

-- The chars are stored in order of priority.
getItemPriority :: BackpackItem -> Int
getItemPriority item =
  case item `elemIndex` priorityScores of
    Just index -> index + 1
    Nothing    -> error "Invalid backpack item, not one of a-zA-Z"
  where
    priorityScores = ['a'..'z'] <> ['A'..'Z']

-- Finds element which exists in two lists
findCommonElem :: Eq a => Show a => [a] -> [a] -> a
findCommonElem listA (nextItem:restOfListB) =
  if nextItem `elem` listA
     then nextItem
     else findCommonElem listA restOfListB
findCommonElem listA listB@[] =
  error ("No item appeared in both lists . List A " ++ show listA ++ ". List B: " ++ show listB)

