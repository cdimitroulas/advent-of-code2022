module Solutions.Day04.Part1 where

import           Data.List              (isInfixOf)
import           Lib.Common             (solve)
import           Solutions.Day04.Common (Range, parseLines)

rangeIsContained :: (Range, Range) -> Bool
rangeIsContained ((xStart, xEnd), (yStart, yEnd)) =
  xElems `isInfixOf` yElems || yElems `isInfixOf` xElems
  where
    xElems = [xStart..xEnd]
    yElems = [yStart..yEnd]

solution_1 :: IO ()
solution_1 = do
  result <- solve "data/day04.txt" parseLines solver
  print result
    where
      solver = length . filter rangeIsContained
