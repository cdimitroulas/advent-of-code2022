module Solutions.Day5.Common where

import           Control.Applicative  ((<|>))
import           Control.Monad
import           Data.Attoparsec.Text (IResult (Done, Fail), Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Char            (digitToInt, isSpace)
import           Data.Either          (isRight)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Lib.Common           (filterMaybe)

type Crate = Char

type Stack = [Crate]

data Instruction = Instruction { amount :: Int, startCol :: Int, endCol :: Int } deriving (Show)

-- 3 space chars denote a lack of crate at a certain height of the stack
noCrateParser :: Parser (Maybe Crate)
noCrateParser = do
  P.count 3 P.space
  void P.space <|> P.endOfInput
  return Nothing

crateParser :: Parser (Maybe Crate)
crateParser = do
  P.char '['
  crateChar <- P.letter
  P.char ']'
  -- Optional space
  void P.space <|> P.endOfInput
  return $ Just crateChar

instructionParser :: Parser Instruction
instructionParser = do
  P.string "move "
  amount <- P.decimal
  P.string " from "
  startCol <- P.decimal
  P.string " to "
  endCol <- P.decimal
  return $ Instruction {amount=amount, startCol=startCol, endCol=endCol}

parseCrate :: Text -> P.Result (Maybe Crate)
parseCrate = P.parse (crateParser <|> noCrateParser)

parseCrateLine :: Text -> [Either String (Maybe Crate)]
parseCrateLine txt = case parseCrate txt of
                       Done remaining crate -> Right crate : parseCrateLine remaining
                       Fail _ _ msg         -> [Left msg]
                       partial                    -> case P.feed partial "" of
                                                       Done _ crate -> [Right crate]
                                                       Fail _ _ msg -> [Left msg]
                                                       _            -> []

-- " 1   2   3   4   5 " -> [1, 2, 3, 4, 5]
parseColNums :: Text -> [Int]
parseColNums = map digitToInt . T.unpack . T.filter (not . isSpace)

parseCrateSection :: Text -> Map Int Stack
parseCrateSection txt = toStacks 0 M.empty $ map parseCrateLine (init crateLines)
  where
    crateLines = T.lines txt

    totalCols = subtract 1 $ last $ parseColNums (last crateLines)

    toStack :: Int -> [[Either String (Maybe Crate)]] -> Stack
    toStack colNum =
      filterMaybe
      . map (\(Right crate) -> crate)
      -- filter out any empty values, leaving us with the crate characters
      . filter isRight
      -- Take the nth value from each row, which is equivalent to getting a column/stack
      . map (!! colNum)

    toStacks :: Int -> Map Int [Crate] -> [[Either String (Maybe Crate)]] -> Map Int [Crate]
    toStacks colNum stackMap crates
      | colNum <= totalCols =
        toStacks
          (colNum + 1)
          (flip (M.insert (colNum + 1)) stackMap . toStack colNum $ crates)
          crates
      | otherwise = stackMap

parser :: Text -> Either String (Map Int Stack, [Instruction])
parser =
    (\(crateStr:instructionStr:_) ->
        combine (parseInstructions instructionStr) (parseCrateSection crateStr))
    . T.splitOn "\n\n"
  where
    parseInstructions = mapM (P.parseOnly instructionParser) . T.lines

    combine :: Either String [Instruction] -> Map Int Stack -> Either String (Map Int Stack, [Instruction])
    combine (Right instructions) stacks = Right (stacks, instructions)
    combine (Left err) _                = Left err

getAnswer :: Map Int Stack -> [Char]
getAnswer = map (head . snd) . M.toList

