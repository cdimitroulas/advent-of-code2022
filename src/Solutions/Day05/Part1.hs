module Solutions.Day05.Part1 where

import           Data.Map               (Map)
import qualified Data.Map               as M
import qualified Data.Text.IO           as TIO
import           Solutions.Day05.Common

popFromStack :: Stack -> (Maybe Crate, Stack)
popFromStack (crate:newStack) = (Just crate, newStack)
popFromStack stack            = (Nothing, stack)

pushToStack :: Crate -> Stack -> Stack
pushToStack crate stack = crate : stack

runInstruction :: Instruction -> Map Int Stack -> Map Int Stack
runInstruction Instruction{..} stacks
  | amount == 0 || length startingStack < amount = stacks
  | otherwise =
      runInstruction
        (Instruction { amount = amount - 1, startCol = startCol, endCol = endCol })
        newStacks
      where
        startingStack = (M.!) stacks startCol
        endingStack = (M.!) stacks endCol
        (crateToMove, newStartingStack) = popFromStack startingStack
        newEndingStack = case crateToMove of
                           Just crate -> pushToStack crate endingStack
                           Nothing    -> endingStack
        newStacks =
          M.insert endCol newEndingStack $
            M.insert startCol newStartingStack stacks

solve :: (Map Int Stack, [Instruction]) -> [Char]
solve (stacks, instructions) = getAnswer . runInstructions $ instructions
  where
    runInstructions = foldl (flip runInstruction) stacks

solution_1 :: IO ()
solution_1 = do
  parseResult <- parser <$> TIO.readFile "data/day05.txt"
  case parseResult of
    Right x -> print $ solve x
    Left x  -> print x
