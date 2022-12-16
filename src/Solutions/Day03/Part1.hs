module Solutions.Day03.Part1 (solution_1) where

import           Lib.Common             (solve)
import           Solutions.Day03.Common (Backpack, BackpackItem, findCommonElem, getItemPriority,
                                         parseBackpack)

findItemTypeInBothCompartments :: Backpack -> BackpackItem
findItemTypeInBothCompartments (firstCompItems, sndCompItems) =
  findCommonElem firstCompItems sndCompItems

solution_1 :: IO (Maybe Int)
solution_1 = solve "data/day3.txt" (mapM $ Just . parseBackpack) solver
  where
    solver = sum . map (getItemPriority . findItemTypeInBothCompartments)


