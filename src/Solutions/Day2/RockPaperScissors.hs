module Solutions.Day2.RockPaperScissors (solution_1) where

import           Control.Monad        ((>=>))
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parser
import           Data.Text            (Text)
import           Lib.Common           (solve)

data GameChoice = Rock | Paper | Scissors deriving (Bounded, Eq, Enum, Show)

data GameResult = Win | Lose | Draw deriving (Show)

data GameStrategy = GameStrategy { opponentChoice :: GameChoice, yourChoice :: GameChoice }

-- Given a game choice, what is the correct choice in order to win?
winningChoice :: GameChoice -> GameChoice
winningChoice Scissors   = Rock
winningChoice gameChoice = succ gameChoice

playGame :: GameStrategy -> GameResult
playGame GameStrategy{..}
  | opponentChoice == yourChoice = Draw
  | winningChoice opponentChoice == yourChoice = Win
  | otherwise = Lose

choiceScore :: GameChoice -> Int
choiceScore Rock     = 1
choiceScore Paper    = 2
choiceScore Scissors = 3

resultScore :: GameResult -> Int
resultScore Win  = 6
resultScore Draw = 3
resultScore Lose = 0

calcScore :: GameStrategy -> Int
calcScore strategy@GameStrategy{..} = resultScore (playGame strategy) + choiceScore yourChoice

solve_1 :: [GameStrategy] -> Int
solve_1 = foldl (\total strategy -> total + calcScore strategy) 0

solution_1 :: IO (Maybe Int)
solution_1 = solve "data/day2.txt" parser solve_1

data GameStrategy2 = GameStrategy2 { opponentChoice2 :: GameChoice, desiredResult :: GameResult }

parseResult :: Char -> Maybe GameResult
parseResult 'X' = Just Lose
parseResult 'Y' = Just Draw
parseResult 'Z' = Just Win
parseResult _   = Nothing

parseGameStrategy2 :: (Char, Char) -> Maybe GameStrategy2
parseGameStrategy2 (opponentLetter, resultLetter) =
  GameStrategy2 <$> parseChoice opponentLetter <*> parseResult resultLetter

parser2 :: [Text] -> Maybe [GameStrategy2]
parser2 = mapM _parseLine . filter (/= "")
  where
    _parseLine = Parser.maybeResult . Parser.parse parseLine >=> parseGameStrategy2

-- Calculates what game choice you need to make to get the desired result
calcChoice :: GameStrategy2 -> GameChoice
calcChoice GameStrategy2{ opponentChoice2, desiredResult = Draw } = opponentChoice2
calcChoice GameStrategy2{ opponentChoice2, desiredResult = Lose } =
  winningChoice $ winningChoice opponentChoice2
calcChoice GameStrategy2{ opponentChoice2, desiredResult = Win } =
  winningChoice opponentChoice2

calcScore2 :: GameStrategy2 -> Int
calcScore2 strategy@GameStrategy2{..} =
  resultScore desiredResult + choiceScore (calcChoice strategy)

solve_2 :: [GameStrategy2] -> Int
solve_2 = foldl (\total strategy -> total + calcScore2 strategy) 0

solution_2 :: IO (Maybe Int)
solution_2 = solve "data/day2.txt" parser2 solve_2

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

parseGameStrategy :: (Char, Char) -> Maybe GameStrategy
parseGameStrategy (opponentLetter, yourLetter) =
  GameStrategy <$> parseChoice opponentLetter <*> parseChoice yourLetter

parseLine :: Parser (Char, Char)
parseLine = do
  opponentLetter <- Parser.letter
  Parser.space
  yourLetter <- Parser.letter
  return (opponentLetter, yourLetter)

parser :: [Text] -> Maybe [GameStrategy]
parser = mapM _parseLine . filter (/= "")
  where
    _parseLine = Parser.maybeResult . Parser.parse parseLine >=> parseGameStrategy
