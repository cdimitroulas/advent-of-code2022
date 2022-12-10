module Lib.Parsing where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Char            (isSpace)
import           Data.Text            (Text)

skipRestOfLine :: Parser ()
skipRestOfLine = P.skipWhile (not . P.isEndOfLine) >> (P.endOfLine <|> P.endOfInput)

word :: Parser Text
word = P.takeWhile (not . isSpace)
