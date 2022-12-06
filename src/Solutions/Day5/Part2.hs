module Solutions.Day5.Part1 where

import           Data.Map              (Map)
import qualified Data.Map              as M
import qualified Data.Text.IO          as TIO
import           Solutions.Day5.Common

takeFromStack :: Int -> Stack -> ([Crate], Stack)
takeFromStack = splitAt

pushToStack :: [Crate] -> Stack -> Stack
pushToStack crates stack = crates <> stack

runInstruction' :: Instruction -> Map Int Stack -> Map Int Stack
runInstruction' Instruction{..} stacks = newStacks
  where
    startingStack = (M.!) stacks startCol
    endingStack = (M.!) stacks endCol
    (cratesToMove, newStartingStack) = takeFromStack amount startingStack
    newEndingStack = pushToStack cratesToMove endingStack
    newStacks =
      M.insert endCol newEndingStack $
        M.insert startCol newStartingStack stacks

solve :: (Map Int Stack, [Instruction]) -> [Char]
solve (stacks, instructions) = getAnswer . runInstructions $ instructions
  where
    runInstructions = foldl (flip runInstruction') stacks

solution_2 :: IO ()
solution_2 = do
  parseResult <- parser <$> TIO.readFile "data/day5.txt"
  case parseResult of
    Right x -> print $ solve x
    Left x  -> print x
