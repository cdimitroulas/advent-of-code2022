module Solutions.Day7.Common where

import           Control.Applicative
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Tree            (Tree)
import           Data.Vector          (Vector)
import qualified Data.Vector          as V

data TerminalLine = Command Cmd | DirInfo Text | FileInfo Int deriving (Show)

data Cmd = ChangeDir Text | ListDir deriving (Show)

data FileTree = Dir Text (Vector FileTree) | File Int deriving (Show)

isDirectory :: FileTree -> Bool
isDirectory (Dir _ _) = True
isDirectory (File _ ) = False

isFile :: FileTree -> Bool
isFile (Dir _ _) = False
isFile (File _ ) = True

-- Adds contents to the directory node. If given a file node, returns it without modifying
addFileNode :: FileTree -> FileTree -> FileTree
addFileNode (Dir dirname dirContents) newContent =
  Dir dirname (newContent `V.cons` dirContents)
addFileNode file@(File _) _                      = file

containsDir :: Text -> FileTree -> Bool
containsDir searchDirName (Dir dirName contents) =
  dirName == searchDirName || any (containsDir searchDirName) contents
containsDir _ (File _) = False

partitionFileNodes :: [FileTree] -> ([Int], [FileTree])
partitionFileNodes = loop ([], [])
  where
    loop (files, dirs) ((File size):xs) = loop (files <> [size], dirs) xs
    loop (files, dirs) (dir:xs)         = loop (files, dirs <> [dir]) xs
    loop result []                      = result

fileParser :: Parser TerminalLine
fileParser = do
  size <- P.decimal
  return $ FileInfo size

dirParser :: Parser TerminalLine
dirParser = do
  P.string "dir"
  P.space
  dirName <- P.take 1
  return $ DirInfo dirName

changeDirParser :: Parser TerminalLine
changeDirParser = do
  P.string "$ cd "
  dirName <- P.take 1
  return $ Command $ ChangeDir dirName

listDirParser :: Parser TerminalLine
listDirParser = do
  P.string "$ ls"
  return $ Command ListDir

terminalLineParser :: Parser TerminalLine
terminalLineParser = changeDirParser <|> listDirParser <|> dirParser <|> fileParser

type Crumb = FileTree
type DirBreadcrumbs = [Crumb]

processTermLine :: (FileTree, DirBreadcrumbs) -> TerminalLine -> (FileTree, DirBreadcrumbs)
processTermLine (tree, breadcrumbs) (Command (ChangeDir dirname)) =
  if dirname == ".."
     then ((head breadcrumbs), tail breadcrumbs)
     else (Dir dirname [], tree:breadcrumbs)
processTermLine (tree, breadcrumbs) (Command ListDir) =
  (tree, breadcrumbs)
processTermLine (tree, breadcrumbs) (DirInfo dirName) =
  (
    addFileNode tree (Dir dirName []),
    breadcrumbs
  )
processTermLine (tree, breadcrumbs) (FileInfo fileSize) =
  (
    addFileNode tree (File fileSize),
    breadcrumbs
  )

buildFileTree :: [TerminalLine] -> FileTree
buildFileTree = fst . loop (Dir "/" [], [])
  where
    loop :: (FileTree, DirBreadcrumbs) -> [TerminalLine] -> (FileTree, DirBreadcrumbs)
    loop state (nextLine:ls) = loop (processTermLine state nextLine) ls
    loop state []            = state
    -- loop ((Command (ChangeDir dirName)):xs) breadcrumbs = loop xs [dirname]



-- cdLsCmdParser :: Parser FileNode
-- cdLsCmdParser = do
--   P.string "$ cd "
--   dirName <- P.take 1
--   skipRestOfLine
--   P.string "$ ls"
--   skipRestOfLine
--   dirContents <- P.many1 (fileParser <|> dirParser)
--   return $ Dir dirName dirContents

-- cdUpDirCmdParser :: Parser ()
-- cdUpDirCmdParser = void $ P.many1 (P.string "$ cd .." >> skipRestOfLine)

-- combineParsedFileNodes :: [FileNode] -> FileNode
-- combineParsedFileNodes = loop "/"
--   where
--     getTotalDirContents :: Text -> [FileNode] -> FileNode
--     -- TODO: write a fn to combine all the
--     getTotalDirContents dirName nodes = _

--     loop :: Text -> [FileNode] -> FileNode
--     loop dirName []    = _
--     loop dirName nodes = filter (containsDir dirName) nodes

-- findAllDirVals :: Text -> [FileNode] -> FileNode
-- findAllDirVals dirName nodes = _

run :: IO ()
run = do
  fileData <- mapM (P.parseOnly terminalLineParser) . T.lines <$> TIO.readFile "data/day7-example.txt"
  -- print fileData
  print $ do
    buildFileTree <$> fileData
  -- print $ do
  --   parseResult <- P.parseOnly parser fileData
  --   return $ containsDir "e" $ head parseResult
  -- case parseResult of
  --   Right result -> do
  --     print $ containsDir "/" result
  --   Left msg     -> print $ "Parsing failed: " ++ msg
