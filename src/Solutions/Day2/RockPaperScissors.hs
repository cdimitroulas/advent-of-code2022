module Solutions.Day2.RockPaperScissors (solution_1) where

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import           Data.Text            (Text)
import           Lib.Common           (solve)

data GameChoice = Rock | Paper | Scissors deriving (Bounded, Eq, Enum, Show)

data GameResult = Win | Lose | Draw deriving (Show)

data GameStrategy = GameStrategy { opponentChoice :: GameChoice, yourChoice :: GameChoice }

choiceScore :: GameChoice -> Int
choiceScore Rock     = 1
choiceScore Paper    = 2
choiceScore Scissors = 3

resultScore :: GameResult -> Int
resultScore Win  = 6
resultScore Draw = 3
resultScore Lose = 0

-- Given a game choice, what is the correct choice in order to win?
winningChoice :: GameChoice -> GameChoice
winningChoice Scissors   = Rock
-- The GameChoices are written in order so that the successor wins against it's predecessor
winningChoice gameChoice = succ gameChoice

-- Returns the game result given a game strategy
strategyResult :: GameStrategy -> GameResult
strategyResult GameStrategy{..}
  | opponentChoice == yourChoice = Draw
  | winningChoice opponentChoice == yourChoice = Win
  | otherwise = Lose

solution_1 :: IO (Maybe Int)
solution_1 = solve "data/day2.txt" parser (sum . map calcScore)
  where
    parser = mapM parseGameStrategy . filter (/= "")
    calcScore strategy@GameStrategy{..} =
      resultScore (strategyResult strategy) + choiceScore yourChoice


data GameStrategy2 = GameStrategy2 { opponentChoice2 :: GameChoice, desiredResult :: GameResult }

parseResult :: Char -> Maybe GameResult
parseResult 'X' = Just Lose
parseResult 'Y' = Just Draw
parseResult 'Z' = Just Win
parseResult _   = Nothing

-- Calculates what game choice you need to make to get the desired result
choiceForDesiredResult :: GameStrategy2 -> GameChoice
choiceForDesiredResult GameStrategy2{ opponentChoice2, desiredResult = Draw } = opponentChoice2
choiceForDesiredResult GameStrategy2{ opponentChoice2, desiredResult = Lose } =
  winningChoice $ winningChoice opponentChoice2
choiceForDesiredResult GameStrategy2{ opponentChoice2, desiredResult = Win } =
  winningChoice opponentChoice2

solution_2 :: IO (Maybe Int)
solution_2 = solve "data/day2.txt" parser (sum . map calcScore)
  where
    parser = mapM parseGameStrategy2 . filter (/= "")
    calcScore strategy@GameStrategy2{..} =
      resultScore desiredResult + choiceScore (choiceForDesiredResult strategy)

---- Parsing
--
--
parseChoice :: Char -> Maybe GameChoice
parseChoice 'A' = Just Rock
parseChoice 'B' = Just Paper
parseChoice 'C' = Just Scissors
parseChoice 'X' = Just Rock
parseChoice 'Y' = Just Paper
parseChoice 'Z' = Just Scissors
parseChoice _   = Nothing

lineParser :: Parser (Char, Char)
lineParser = do
    opponentLetter <- Parser.letter
    Parser.space
    yourLetter <- Parser.letter
    return (opponentLetter, yourLetter)

parseGameStrategy :: Text -> Maybe GameStrategy
parseGameStrategy line = do
  (opponentLetter, yourLetter) <- Parser.maybeResult . Parser.parse lineParser $ line
  GameStrategy <$> parseChoice opponentLetter <*> parseChoice yourLetter

parseGameStrategy2 :: Text -> Maybe GameStrategy2
parseGameStrategy2 line = do
  (opponentLetter, resultLetter) <- Parser.maybeResult . Parser.parse lineParser $ line
  GameStrategy2 <$> parseChoice opponentLetter <*> parseResult resultLetter
