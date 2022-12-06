module Solutions.Day6.Common where

import           Data.List (nub)

isUniqueList :: Eq a => [a] -> Bool
isUniqueList list = nub list == list

-- returns a tuple containing the chars + the index of the last char
findFirstNUniqueElems :: (Eq a, Ord a) => Int -> [a] -> Maybe ([a], Int)
findFirstNUniqueElems num list = go list num
  where
    go :: (Eq a, Ord a) => [a] -> Int -> Maybe ([a], Int)
    go elems currentPosition =
      if isUniqueList sublist
         then Just (sublist, currentPosition)
         else go (tail sublist <> restOfList) (currentPosition + 1)
           where
             (sublist, restOfList) = splitAt num elems
