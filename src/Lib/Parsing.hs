module Lib.Parsing where

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P

skipRestOfLine :: Parser ()
skipRestOfLine = P.skipWhile (not . P.isEndOfLine) >> P.endOfLine
