module Solutions.Day12 (main) where

import           Algorithm.Search (bfs)
import           Data.Array       ((!))
import qualified Data.Array       as A
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Lib.Common       (mapWithIndex)
import           Lib.Grid         (Grid2)
import qualified Lib.Grid         as G

parse :: Text -> Grid2 Char
parse =
  (\list -> A.array (bounds list) list)
    . concat
    . mapWithIndex parseLine
    . T.lines
  where
    parseLine :: Int -> Text -> [(G.Coord, Char)]
    parseLine y = mapWithIndex (\x c -> ((x, y), c)) . T.unpack
    bounds list = (fst $ head list, fst $ last list)

canMoveTo :: Char -> Char -> Bool
canMoveTo 'y' 'E'        = True
canMoveTo 'z' 'E'        = True
canMoveTo 'S' 'a'        = True
canMoveTo 'S' 'b'        = True
canMoveTo current target = succ current == target || current == target

-- Returns a list of the next possible directions that can be moved to from the current
-- position
nextDirections :: Grid2 Char -> G.Coord -> [G.Coord]
nextDirections grid pos =
  map fst . filter canMoveTo' $ G.adjacentPositionsAssoc grid pos
  where
    currentHeight = grid ! pos
    canMoveTo' = canMoveTo currentHeight . snd

part1 :: Grid2 Char -> Int
part1 grid = case bfs (nextDirections grid) (== destPos) startPos of
               Just path -> length path
               Nothing   -> error "No path found from 'S' to 'E'"
  where
    startPos = G.findCoord' (== 'S') grid
    destPos = G.findCoord' (== 'E') grid

-- part2 = error "Not implemented"

-- solution = Solution (Right . parse) part1 part2

main :: IO ()
main = do
  input <- parse <$> TIO.readFile "data/day12.txt"
  print $ part1 input
