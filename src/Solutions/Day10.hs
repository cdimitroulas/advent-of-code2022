module Solutions.Day10 where
import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
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
          (\cycle -> (cycle + 1) * (regVals !! cycle))
          (map (subtract 1) importantCycles)
