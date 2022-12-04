module Solutions.Day2.Part2 (solution_2) where

import qualified Data.Attoparsec.Text  as Parser
import           Data.Text             (Text)
import           Lib.Common            (solve)
import           Solutions.Day2.Common (GameChoice, GameResult (..),
                                        choiceScore, lineParser, parseChoice,
                                        parseResult, resultScore, winningChoice)

data GameStrategy = GameStrategy { opponentChoice :: GameChoice, desiredResult :: GameResult }

-- Calculates what game choice you need to make to get the desired result
choiceForDesiredResult :: GameStrategy -> GameChoice
choiceForDesiredResult GameStrategy{ opponentChoice, desiredResult = Draw } = opponentChoice
choiceForDesiredResult GameStrategy{ opponentChoice, desiredResult = Lose } =
  winningChoice $ winningChoice opponentChoice
choiceForDesiredResult GameStrategy{ opponentChoice, desiredResult = Win } =
  winningChoice opponentChoice

parseGameStrategy :: Text -> Maybe GameStrategy
parseGameStrategy line = do
  (opponentLetter, resultLetter) <- Parser.maybeResult . Parser.parse lineParser $ line
  GameStrategy <$> parseChoice opponentLetter <*> parseResult resultLetter

solution_2 :: IO (Maybe Int)
solution_2 = solve "data/day2.txt" parser (sum . map calcScore)
  where
    parser = mapM parseGameStrategy
    calcScore strategy@GameStrategy{..} =
      resultScore desiredResult + choiceScore (choiceForDesiredResult strategy)
