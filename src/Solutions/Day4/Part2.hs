module Solutions.Day4.Part2 where

import           Lib.Common            (solve)
import           Solutions.Day4.Common (Range, parseLines)

rangesOverlap :: (Range, Range) -> Bool
rangesOverlap ((xStart, xEnd), (yStart, yEnd)) =
  (yStart `elem` xElems || yEnd `elem` xElems) || (xStart `elem` yElems || xEnd `elem` yElems)
  where
    xElems = [xStart..xEnd]
    yElems = [yStart..yEnd]

solution_2 :: IO ()
solution_2 = do
  result <- solve "data/day4.txt" parseLines solver
  print result
    where
      solver = length . filter rangesOverlap
