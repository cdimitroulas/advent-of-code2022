module Lib.Grid where

import           Data.Array (Array)
import qualified Data.Array as A
import           Data.List  (find)
import           Data.Maybe (catMaybes)

type Coord = (Int, Int) -- (X, Y)

type Grid2 a = Array Coord a

maxBounds :: Grid2 a -> Coord
maxBounds = snd . A.bounds

-- total version of A.!
(!?) :: Grid2 a -> Coord -> Maybe a
(!?) grid pos@(x, y)
  | x <= maxX && y <= maxY = Just (grid A.! pos)
  | otherwise = Nothing
    where
      (maxX, maxY) = maxBounds grid

findCoord :: Eq a => (a -> Bool) -> Grid2 a -> Maybe Coord
findCoord f grid = fst <$> find (f . snd) (A.assocs grid)

-- unsafe version of findCoord
findCoord' :: Eq a => (a -> Bool) -> Grid2 a -> Coord
findCoord' grid item = case findCoord grid item of
                         Just x  -> x
                         Nothing -> error "Could not find desired element in grid"

adjacentPositions :: Grid2 a -> Coord -> [Coord]
adjacentPositions grid (x, y) = catMaybes [upPosM, downPosM, leftPosM, rightPosM]
  where
    (maxX, maxY) = maxBounds grid
    upPosM = if y > 0 then Just (x, y - 1) else Nothing
    downPosM = if y < maxY then Just (x, y + 1) else Nothing
    leftPosM = if x > 0 then Just (x - 1, y) else Nothing
    rightPosM = if x < maxX then Just (x + 1, y) else Nothing

adjacentPositionsAssoc :: Grid2 a -> Coord -> [(Coord, a)]
adjacentPositionsAssoc grid (x, y) = map (\coord -> (coord, grid A.! coord)) $ catMaybes [upPosM, downPosM, leftPosM, rightPosM]
  where
    (maxX, maxY) = maxBounds grid
    upPosM = if y > 0 then Just (x, y - 1) else Nothing
    downPosM = if y < maxY then Just (x, y + 1) else Nothing
    leftPosM = if x > 0 then Just (x - 1, y) else Nothing
    rightPosM = if x < maxX then Just (x + 1, y) else Nothing
