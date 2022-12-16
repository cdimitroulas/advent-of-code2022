module Solutions.Day02.Part1 where

import qualified Data.Attoparsec.Text   as Parser
import           Data.Text              (Text)
import           Lib.Common             (solve)
import           Solutions.Day02.Common (GameChoice, GameResult (..), choiceScore, lineParser,
                                         parseChoice, resultScore, winningChoice)

data GameStrategy = GameStrategy { opponentChoice :: GameChoice, yourChoice :: GameChoice }

parseGameStrategy :: Text -> Maybe GameStrategy
parseGameStrategy line = do
  (opponentLetter, yourLetter) <- Parser.maybeResult . Parser.parse lineParser $ line
  GameStrategy <$> parseChoice opponentLetter <*> parseChoice yourLetter

-- Returns the game result given a game strategy
strategyResult :: GameStrategy -> GameResult
strategyResult GameStrategy{..}
  | opponentChoice == yourChoice = Draw
  | winningChoice opponentChoice == yourChoice = Win
  | otherwise = Lose

solution_1 :: IO (Maybe Int)
solution_1 = solve "data/day2.txt" parser (sum . map calcScore)
  where
    parser = mapM parseGameStrategy
    calcScore strategy@GameStrategy{..} =
      resultScore (strategyResult strategy) + choiceScore yourChoice
