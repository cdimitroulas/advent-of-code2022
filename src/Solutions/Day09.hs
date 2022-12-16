module Solutions.Day09 where
import           Control.Applicative
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.List            (foldl')
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Lib.Common           (uniq)

data Direction = UpDir | DownDir | LeftDir | RightDir deriving (Show, Eq)

data Position = Position { posX :: Int, posY :: Int } deriving (Show, Eq)

type Rope = [Position]

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
handleInstruction _ [] = []
handleInstruction _ [x] = [x]
handleInstruction direction (headPos : (tailPos:restOfRope)) =
  newHeadPos : reverse (foldl'
                  (\rope nextKnot -> calcTailPos (head rope) nextKnot : rope)
                  [calcTailPos newHeadPos tailPos]
                  restOfRope)
  where
    newHeadPos = move headPos direction

    calcTailPos :: Position -> Position -> Position
    calcTailPos headP tailP
      | positionsAreTouching headP tailP = tailP
      | diffPos.posX == 0 =
          tailP {
            posY = (if diffPos.posY > 0 then (+) else (-)) tailP.posY 1
          }
      | diffPos.posY == 0 =
          tailP {
            posX = (if diffPos.posX > 0 then (+) else (-)) tailP.posX 1
          }
      -- Case where they aren't in same row or column
      | otherwise =
          tailP {
            posX = (if diffPos.posX >= 0 then (+) else (-)) tailP.posX 1,
            posY = (if diffPos.posY >= 0 then (+) else (-)) tailP.posY 1
          }
        where
          diffPos = positionDiff headP tailP

initialRope :: Int -> Rope
initialRope numOfKnots = replicate numOfKnots (Position 0 0 )

solve :: Int -> [Direction] -> Int
solve numOfKnots =
  length
    . uniq
    . map last
    . snd
    . loop (initialRope numOfKnots, [])
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
  return $ solve 2 <$> instructions

solution_2 :: IO (Either String Int)
solution_2 = do
  instructions <- parseInstructions <$> TIO.readFile "data/day9.txt"
  return $ solve 10 <$> instructions

