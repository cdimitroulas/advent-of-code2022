module Lib.AOC (runFirstPartSolution, runSolution) where

import           Data.Text    (Text)
import qualified Data.Text.IO as TIO

runFirstPartSolution ::
  Show input => Show output => Monad m => Show (m output) =>
    String
    -- ^ the day number as a string (e.g. "01" or "11")
    -> (Text -> m input)
    -- ^ parser
    -> (input -> output)
    -- ^ the part1 solver fn
    -> IO ()
runFirstPartSolution day parser solve = do
  parsed <- parser <$> TIO.readFile ("data/day" ++ day ++ ".txt")
  putStrLn "Part 1:"
  print $ solve <$> parsed

runSolution ::
  Show input => Show output => Monad m => Show (m output) =>
  String
  -- ^ the day number as a string (e.g. "01" or "11")
  -> (Text -> m input)
  -- ^ parser
  -> (input -> output)
  -- ^ the part1 solver fn
  -> (input -> output)
  -- ^ the part2 solver fn
  -> IO ()
runSolution day parser part1 part2 = do
  parsed <- parser <$> TIO.readFile ("data/day" ++ day ++ ".txt")
  putStrLn "Part 1:"
  print $ part1 <$> parsed
  putStrLn "Part 2:"
  print $ part2 <$> parsed
