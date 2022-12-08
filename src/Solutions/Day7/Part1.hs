module Solutions.Day7.Part1 where

import           Control.Applicative
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Char            (isAlphaNum)
import           Data.List            (partition)
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Lib.Parsing          (skipRestOfLine)

data Tree a = Branch a [Tree a] | Leaf a deriving (Show, Eq, Functor, Foldable, Traversable)

isLeaf :: Tree a -> Bool
isLeaf (Branch _ _) = False
isLeaf (Leaf _)     = True

data FileItem = FIDir Text | FIFile Int deriving (Show, Eq)

isFile :: FileItem -> Bool
isFile (FIDir _)  = False
isFile (FIFile _) = True

type FileSystem = Tree FileItem

data TerminalLine = CdCmd Text | CdUpCmd | LsCmd [FileItem] deriving (Show, Eq)

fileParser :: Parser FileItem
fileParser = FIFile <$> P.decimal <* skipRestOfLine

dirParser :: Parser FileItem
dirParser = FIDir <$> (P.string "dir" *> P.space *> P.takeWhile1 isAlphaNum <* skipRestOfLine)

changeDirParser :: Parser TerminalLine
changeDirParser = CdCmd <$> (P.string "$ cd " *> cdTarget <* skipRestOfLine)
  where
    cdTarget = P.takeWhile1 isAlphaNum <|> P.string "/"

changeDirUpParser :: Parser TerminalLine
changeDirUpParser = CdUpCmd <$ (P.string "$ cd .." <* skipRestOfLine)

listDirParser :: Parser TerminalLine
listDirParser = do
  P.string "$ ls"
  skipRestOfLine
  fileItems <- P.many1 (fileParser <|> dirParser)
  return $ LsCmd fileItems

-- TODO get this logic correct - currently it doesn't work. not sure how to handle the CdUpCmd
-- correctly.
cmdsToTree :: [TerminalLine] -> FileSystem
cmdsToTree (CdUpCmd:tLines) = _
-- Here we rely on the fact that a cd cmd is always followed by an ls cmd
cmdsToTree [CdCmd dirName, LsCmd fileItems] =
  Branch (FIDir dirName) children
  where
    children :: [FileSystem]
    children = map Leaf . filter isFile $ fileItems
-- Here we rely on the fact that a cd cmd is always followed by an ls cmd
cmdsToTree xs@((CdCmd dirName):(LsCmd fileItems):tLines) =
  Branch (FIDir dirName) children
  where
    children :: [FileSystem]
    children =
      (\files -> cmdsToTree tLines : map Leaf files). filter isFile $ fileItems

dirs :: FileSystem -> [FileSystem]
dirs branch@(Branch _ dirContents) = branch : concatMap dirs dirContents
dirs (Leaf _)                      = []

dirSize :: FileSystem -> Int
dirSize (Branch _ dirContents) =
  sum (map (\(Leaf (FIFile size)) -> size) fileLeaves) + sum (map dirSize dirBranches)
  where
    (fileLeaves, dirBranches) = partition isLeaf dirContents
dirSize (Leaf (FIFile size))   = size
dirSize (Leaf _)               = 0

run :: IO ()
run = do
  fileData <- parse <$> TIO.readFile "data/day7-example.txt"
  let tree = cmdsToTree <$> fileData

  let sumResult = sum . filter (<= 100_000) . map dirSize . dirs <$> tree
  print sumResult
  where
    parse = P.parseOnly $ P.many1 (changeDirParser <|> changeDirUpParser <|> listDirParser)
