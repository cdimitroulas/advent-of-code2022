module Solutions.Day06.Part2 where

import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Solutions.Day06.Common

solution_2 :: IO ()
solution_2 = do
  -- assumes only one line of input
  [txt] <- filter (/= "") . T.lines <$> TIO.readFile "data/day06.txt"
  print $ findFirstNUniqueElems 14 (T.unpack txt)
