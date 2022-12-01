module Solutions.Day1.CalorieCounting where

import           Data.List  (sort)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Lib.Common (safeTake, solve, splitList)
import           Text.Read  (readMaybe)

type ElfInventory = [Int]

parser :: [Text] -> Maybe [ElfInventory]
parser = mapM parseInventory . splitList ""

parseInventory :: [Text] -> Maybe [Int]
parseInventory = mapM (readMaybe . T.unpack)

-- Returns the highest number of calories carried by any elf
solver :: [ElfInventory] -> Int
solver = maximum . map sum

-- Returns the total calories carried by the 3 elfs with most calories
solver_2 :: [ElfInventory] -> Maybe Int
solver_2 inventories = sum <$> (safeTake 3 . reverse . sort . map sum) inventories

solution :: IO (Maybe Int)
solution = solve "data/day1.txt" parser solver

solution_2 :: IO (Maybe (Maybe Int))
solution_2 = solve "data/day1.txt" parser solver_2

