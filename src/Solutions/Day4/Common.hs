module Solutions.Day4.Common where

import           Control.Monad        ((>=>))
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Lib.Common           (traverseBoth)

type Range = (Int, Int)

rangeParser :: Parser Range
rangeParser = do
  rangeStart <- P.decimal
  P.char '-'
  rangeEnd <- P.decimal
  return (rangeStart, rangeEnd)

-- Runs T.Split but only succeeds if there is exactly one separator char Char
-- e.g. "1234,5678" (with "," as the separator)
split2 :: Char -> Text -> Either String (Text, Text)
split2 separator txt =
  case T.split (== separator) txt of
    [x1, x2] -> Right (x1, x2)
    list        -> Left $
      "List contained more than one separator '" ++ show separator ++ "'. List: "
        ++ show list

parseLines :: [Text] -> Either String [(Range, Range)]
parseLines =  mapM (split2 ',' >=> traverseBoth (P.parseOnly  rangeParser))
