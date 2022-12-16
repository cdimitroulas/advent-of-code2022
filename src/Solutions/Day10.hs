module Solutions.Day10 where
import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Debug.Trace          (traceShowId)
import           Lib.Parsing          (linesOf)

data Instruction = AddX !Int | NoOp deriving (Show)

parseInstructions :: Text -> Either String [Instruction]
parseInstructions = P.parseOnly (linesOf instructionParser)
  where
    instructionParser :: Parser Instruction
    instructionParser = addParser <|> noopParser

    addParser = AddX <$> (P.string "addx " >> P.signed P.decimal)

    noopParser = NoOp <$ P.string "noop"

-- Runs all the instructions, noting the value of register X at each cycle
-- Takes the initial reg value as an arg
getRegisterValues :: [Instruction] -> [Int]
getRegisterValues = scanl (+) 1 . concatMap (\case
  AddX x -> [0, x]
  NoOp -> [0])

-- assume indices are sorted
selectIndices :: [Int] -> [a] -> [a]
selectIndices = go 0
  where go _ [] _ = []
        go i (idx:idxs) (x:xs)
          | i == idx  = x : go (i+1) idxs xs
          | otherwise = go (i+1) (idx:idxs) xs
        go _ _ [] = []

importantCycles :: [Int]
importantCycles = [20, 60..220]

solution_1 :: IO (Either String Int)
solution_1 = do
  instructions <- parseInstructions <$> TIO.readFile "data/day10.txt"

  return $ solve . getRegisterValues <$> instructions

  where
    solve regVals =
      sum $
        map
          (\cycleNum -> (cycleNum + 1) * (regVals !! cycleNum))
          (map (subtract 1) importantCycles)

-- Draws the pixels
-- Accepts the starting position of the sprite as an argument
draw :: Int -> [Int] -> String
draw _ [] = []
draw currentPos (regVal:rest) = pixelStr <> draw (currentPos + 1) rest
  where
    horizontalPos = currentPos `mod` 40
    pixel = if horizontalPos `elem` [regVal - 1, regVal, regVal + 1] then '#' else '.'
    isEndOfLine = horizontalPos == 39
    pixelStr = if isEndOfLine then pixel : "\n" else [pixel]

solution_2 :: IO ()
solution_2 = do
  instructions <- parseInstructions <$> TIO.readFile "data/day10.txt"
  case instructions of
    Right x -> putStr $ draw 0 $ getRegisterValues x
    Left e  -> print e

