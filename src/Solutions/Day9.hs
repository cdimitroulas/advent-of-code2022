module Solutions.Day9 where
import           Control.Applicative
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Lib.Common           (uniq)

data Direction = UpDir | DownDir | LeftDir | RightDir deriving (Show, Eq)

data Position = Position { posX :: Int, posY :: Int } deriving (Show, Eq)

data Rope = Rope { tailPos :: Position, headPos :: Position } deriving (Show)

parseInstructions :: Text -> Either String [Direction]
parseInstructions = P.parseOnly $ concat <$> (instructionParser `P.sepBy` P.endOfLine)
  where
    instructionParser :: Parser [Direction]
    instructionParser = do
      direction <- directionParser
      P.space
      amount :: Integer <- P.decimal
      return $ map (const direction) [0..amount - 1]

    directionParser = (UpDir <$ P.char 'U')
      <|> (DownDir <$ P.char 'D')
      <|> (LeftDir <$ P.char 'L')
      <|> (RightDir <$ P.char 'R')

positionsAreTouching :: Position -> Position -> Bool
positionsAreTouching pos1 pos2 =
  abs (pos1.posX - pos2.posX) <= 1 && abs (pos1.posY - pos2.posY) <= 1

positionDiff :: Position -> Position -> Position
positionDiff pos1 pos2 = Position {
  posX = pos1.posX - pos2.posX,
  posY = pos1.posY - pos2.posY
}

move :: Position -> Direction -> Position
move pos direction
  | direction == UpDir = pos { posY = pos.posY + 1 }
  | direction == DownDir = pos { posY = pos.posY - 1 }
  | direction == LeftDir = pos { posX = pos.posX - 1 }
  | otherwise = pos { posX = pos.posX + 1 }

handleInstruction :: Direction -> Rope -> Rope
handleInstruction direction Rope{..} =
  Rope { tailPos = newTailPos, headPos = newHeadPos }
  where
    newHeadPos = move headPos direction

    diffPos = positionDiff newHeadPos tailPos

    newTailPos
      | positionsAreTouching newHeadPos tailPos = tailPos
      | diffPos.posX == 0 =
          tailPos {
            posY = (if diffPos.posY > 0 then (+) else (-)) tailPos.posY 1
          }
      | diffPos.posY == 0 =
          tailPos {
            posX = (if diffPos.posX > 0 then (+) else (-)) tailPos.posX 1
          }
      -- Case where they aren't in same row or column, and vertical diff is greater
      | otherwise =
          tailPos {
            posX = (if diffPos.posX >= 0 then (+) else (-)) tailPos.posX 1,
            posY = (if diffPos.posY >= 0 then (+) else (-)) tailPos.posY 1
          }

initialRope :: Rope
initialRope = Rope (Position 0 0) (Position 0 0)

solve :: [Direction] -> Int
solve = length . uniq . map tailPos . snd . loop (initialRope, [])
  where
    loop :: (Rope, [Rope]) -> [Direction] -> (Rope, [Rope])
    loop (rope, ropes) [] = (rope, ropes)
    loop (rope, ropes) (nextInstruction:instructions) =
      loop (newRope, newRope : ropes) instructions
      where
        newRope = handleInstruction nextInstruction rope

solution_1 :: IO (Either String Int)
solution_1 = do
  instructions <- parseInstructions <$> TIO.readFile "data/day9.txt"
  return $ solve <$> instructions

