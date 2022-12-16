module Solutions.Day08.Part1 where

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Bifunctor       (Bifunctor (bimap), second)
import           Data.Char            (digitToInt)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Lib.Common           (safeTail)
import           Lib.Matrix           (Matrix, Position (..))
import qualified Lib.Matrix           as Mat

type Trees = Matrix Int

treeLineParser :: Parser [Int]
treeLineParser= P.many1 (digitToInt <$> P.digit)

parse :: Text -> Either String Trees
parse = mapM (P.parseOnly treeLineParser) . filter (/= "") . T.lines

isVisibleTree :: Mat.Position -> Trees -> Bool
isVisibleTree pos@Position{..} trees =
  isAtEdge || any (all (< tree)) [above, below, left, right]
  where
    tree = Mat.getAt pos trees
    isAtEdge =
      posX == 0
        || posY == 0
        || posX == Mat.size trees - 1
        || posY == Mat.size trees - 1
    (above, below) = second safeTail $ splitAt posY (Mat.invert trees !! posX)
    (left, right) = second safeTail $ splitAt posX (trees !! posY)

traverseWithPos :: (Position -> Matrix a -> b) -> Matrix a -> [b]
traverseWithPos f mat = loop (Position 0 0)
  where
    loop pos@Position{..}
      | posX == matSize - 1 && posY == matSize - 1 = [f pos mat]
      | posX == matSize - 1 = f pos mat : loop (Position {posX=0, posY=posY + 1})
      | otherwise = f pos mat : loop (Position {posX=posX+1, posY=posY})
      where
        matSize = Mat.size mat

solution_1 :: IO (Either String Int)
solution_1 = do
  trees <- parse <$> TIO.readFile "data/day8.txt"
  return $ length . filter (== True) . traverseWithPos isVisibleTree <$> trees

calcScenicScore :: Mat.Position -> Trees -> Int
calcScenicScore pos@Position{..} trees =
  product (map (length . takeWhileIncluding (< tree)) [above, below, left, right])
  where
    tree = Mat.getAt pos trees
    -- we need to reverse the elements above it to get them in the right order
    -- and we need to exclude the element itself from the ones below
    (above, below) = bimap reverse safeTail $ splitAt posY (Mat.invert trees !! posX)
    (left, right) = bimap reverse safeTail $ splitAt posX (trees !! posY)

    -- takeWhile which also includes the first element to fail the condition
    takeWhileIncluding fn (x:xs) = if fn x then x : takeWhileIncluding fn xs else [x]
    takeWhileIncluding _ []      = []

solution_2 :: IO (Either String Int)
solution_2 = do
  trees <- parse <$> TIO.readFile "data/day8.txt"
  return $ maximum . traverseWithPos calcScenicScore <$> trees
