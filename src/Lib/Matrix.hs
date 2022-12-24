module Lib.Matrix
  ( Matrix,
    size,
    getCol,
    getRow,
    invert,
    prettyPrint,
    updateRow,
    updateCol,
    Position (..),
    findElemPosition,
    getAt,
    getAt',
    positionIsValid,
    adjacentPositions
  )
  where

import           Data.List  (findIndex, foldl')
import           Lib.Common (setAt, (!!?))

-- A matrix should technically have all rows as equal length but this isn't enforced
-- atm.
-- TODO use a newtype plus specific constructor function to enforce this constraint.
type Matrix a = [[a]]

data Position = Position
                  { posX :: Int
                  , posY :: Int
                  }
  deriving (Eq, Show, Ord)

size :: Matrix a -> Int
size = length

-- Inverts the rows and columns. Useful when you want to perform operations on each column
invert :: Matrix a -> Matrix a
invert matrix = run 0 matrix
  where
    run index mat =
      if index < length (head matrix)
        then getCol index matrix : run (index + 1) mat
        else []

getCol :: Int -> Matrix a -> [a]
getCol pos = map (!! pos)

getRow :: Int -> Matrix a -> [a]
getRow pos = (!! pos)

updateRow :: ([a] -> [a]) -> Int -> Matrix a -> Matrix a
updateRow = setAt

updateCol :: ([a] -> [a]) -> Int -> Matrix a -> Matrix a
updateCol f pos = invert . updateRow f pos . invert

findElemPosition :: (a -> Bool) -> Matrix a -> Maybe Position
findElemPosition f matrix = Position <$> xPosition <*> yPosition
  where
    xPosition :: Maybe Int
    xPosition = (\i y -> i - (rowSize * y)) <$> indexInFlattenedList <*> yPosition
    yPosition :: Maybe Int
    yPosition = (`div` rowSize) <$> indexInFlattenedList
    rowSize = (length . head) matrix
    -- concat matrix flattens the matrix into a flat list to make it easier to work with
    indexInFlattenedList = findIndex f (concat matrix)

-- partial fn
getAt :: Position -> Matrix a -> a
getAt Position{..} mat = mat !! posY !! posX

-- safe version of getAt
getAt' :: Position -> Matrix a -> Maybe a
getAt' Position{..} mat = (mat !!? posY) >>= (!!? posX)

positionIsValid :: Matrix a -> Position -> Bool
positionIsValid mat pos = case getAt' pos mat of
                            Just _  -> True
                            Nothing -> False

adjacentPositions :: Position -> Matrix a -> [Position]
adjacentPositions pos mat = filter (positionIsValid mat) [upPos, downPos, leftPos, rightPos]
  where
    upPos = pos{ posY = pos.posY - 1 }
    downPos = pos{ posY = pos.posY + 1 }
    leftPos = pos{ posX = pos.posX - 1 }
    rightPos = pos{ posX = pos.posX + 1 }

prettyPrint :: Show a => Matrix a -> String
prettyPrint [] = ""
prettyPrint (x : xs) =
  foldl' (\str item -> str <> mkSpacer (show item) <> show item) "" x <> "\n" <> prettyPrint xs
  where
    mkSpacer item = if length item == 2 then " " else "  "
