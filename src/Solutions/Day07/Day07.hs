module Solutions.Day07.Day07 where

import           Control.Applicative
import           Control.Monad        (void)
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Char            (isAlphaNum)
import           Data.List            (find, sort)
import           Data.Maybe           (catMaybes)
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Lib.Parsing          (skipRestOfLine, word)

data FileSystem = FSDir !Text ![FileSystem] | FSFile !Int deriving (Show)

fileParser :: Parser (Maybe FileSystem)
fileParser = (Just . FSFile <$> P.decimal <* P.space <* word
  <|> Nothing <$ P.string "dir " <* word
  <|> Just <$> dirParser) P.<?> "fileParser"

dirParser :: Parser FileSystem
dirParser = do
  -- parse the $ cd <dirname> line
  dirName <- P.string "$ cd " *> dirNameParser <* skipRestOfLine
  -- parse the $ ls line
  P.string "$ ls" <* skipRestOfLine
  -- parse each of the ls cmd output lines using `fileParser`
  files <- catMaybes <$> fileParser `P.sepBy` P.endOfLine <* (P.endOfLine <|> P.endOfInput)
  -- after each cd <dir>, ls, and subsequent list of files outputted by the ls command,
  -- we expect a "$ cd .." command, or the end of the input.
  (void (P.string "$ cd ..") <|> P.endOfInput) P.<?> "cd .."
  return $ FSDir dirName files

  where
    dirNameParser = P.takeWhile1 isAlphaNum <|> P.string "/"

dirs :: FileSystem -> [FileSystem]
dirs dir@(FSDir _ dirContents) = dir : concatMap dirs dirContents
dirs (FSFile _)                = []

dirSize :: FileSystem -> Int
dirSize (FSDir _ dirContents) = sum (map dirSize dirContents)
dirSize (FSFile size)         = size

solution_1 :: IO (Either String Int)
solution_1 = do
  fileData <- P.parseOnly dirParser <$> TIO.readFile "data/day7.txt"
  return $ sum . filter (<= 100_000) . map dirSize . dirs <$> fileData

solution_2 :: IO (Either String Int)
solution_2 = do
  fileData <- P.parseOnly dirParser <$> TIO.readFile "data/day7.txt"
  return $ do
    directories <- dirs <$> fileData
    let currentUnusedSpace = totalDiskSpace - dirSize (head directories)
    let result = find (\x -> x >= (requiredSpace - currentUnusedSpace)) $ sort $ map dirSize directories
    case result of
      Just x  -> return x
      Nothing -> error "did not find any directory big enough to delete"

  where
    totalDiskSpace = 70_000_000
    requiredSpace = 30_000_000
