module Solutions.Day01.CalorieCounting where

import           Data.List  (sort)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Lib.AOC    (runSolution)
import           Lib.Common (safeTake, splitList)
import           Text.Read  (readMaybe)

type ElfInventory = [Int]

parser :: [Text] -> Maybe [ElfInventory]
parser = mapM parseInventory . splitList ""

parseInventory :: [Text] -> Maybe [Int]
parseInventory = mapM (readMaybe . T.unpack)

-- Returns the highest number of calories carried by any elf
part1 :: [ElfInventory] -> Int
part1 = maximum . map sum

-- Returns the total calories carried by the 3 elfs with most calories
part2 :: [ElfInventory] -> Maybe Int
part2 inventories = sum <$> (safeTake 3 . reverse . sort . map sum) inventories

main :: IO ()
main = runSolution "01" (parser . T.lines) (Just <$> part1) part2
