module Solutions.Day4.Part2 where

import           Data.List             (intersect)
import           Lib.Common            (solve)
import           Solutions.Day4.Common (Range, parseLines)

rangesOverlap :: (Range, Range) -> Bool
rangesOverlap ((xStart, xEnd), (yStart, yEnd)) =
  not . null $ [xStart..xEnd] `intersect`  [yStart..yEnd]

solution_2 :: IO ()
solution_2 = do
  result <- solve "data/day4.txt" parseLines solver
  print result
    where
      solver = length . filter rangesOverlap
