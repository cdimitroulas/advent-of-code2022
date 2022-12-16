module Solutions.Day06.Part1 where

import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Solutions.Day06.Common

solution_1 :: IO ()
solution_1 = do
  -- assumes only one line of input
  [txt] <- filter (/= "") . T.lines <$> TIO.readFile "data/day6.txt"
  print $ findFirstNUniqueElems 4 (T.unpack txt)
