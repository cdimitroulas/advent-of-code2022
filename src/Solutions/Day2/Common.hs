module Solutions.Day2.Common where

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P

data GameChoice = Rock | Paper | Scissors deriving (Bounded, Eq, Enum, Show)

data GameResult = Win | Lose | Draw deriving (Show)

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

parseChoice :: Char -> Maybe GameChoice
parseChoice 'A' = Just Rock
parseChoice 'B' = Just Paper
parseChoice 'C' = Just Scissors
parseChoice 'X' = Just Rock
parseChoice 'Y' = Just Paper
parseChoice 'Z' = Just Scissors
parseChoice _   = Nothing

parseResult :: Char -> Maybe GameResult
parseResult 'X' = Just Lose
parseResult 'Y' = Just Draw
parseResult 'Z' = Just Win
parseResult _   = Nothing

lineParser :: Parser (Char, Char)
lineParser = do
    opponentLetter <- P.letter
    P.space
    yourLetter <- P.letter
    return (opponentLetter, yourLetter)

