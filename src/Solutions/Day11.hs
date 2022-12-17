module Solutions.Day11 where

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Foldable        (foldl')
import           Data.List            (sortBy)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Lib.Common           (nTimes)
import           Text.Show.Functions  ()

data Operand = Old | Operand Integer

opToVal :: Operand -> Integer -> Integer
opToVal (Operand x) _ = x
opToVal Old x         = x

newtype MonkeyId = MonkeyId Integer deriving (Show, Eq, Ord)

data Monkey = Monkey
                { monkeyId      :: !MonkeyId
                , startingItems :: ![Integer]
                , operation     :: !(Integer -> Integer)
                , divisor       :: !Integer
                , trueTarget    :: !MonkeyId
                , falseTarget   :: !MonkeyId
                }
  deriving (Show)

type Monkeys = [Monkey]

data MonkeyState = MonkeyState
                     { items      :: ![Integer]
                     , itemCounts :: !Integer
                     }

instance Eq MonkeyState where
  (==) a b = a.itemCounts == b.itemCounts

instance Ord MonkeyState where
  (<=) a b = a.itemCounts <= b.itemCounts

type MonkeyStates = Map MonkeyId MonkeyState

monkeyParser :: Parser Monkey
monkeyParser = do
  monkeyId <- P.string "Monkey " *> P.decimal <* P.char ':' <* P.endOfLine P.<?> "line1"
  items <- P.string "  Starting items: " *> P.many1 (P.decimal <* (void (P.string ", ") <|> P.endOfLine)) P.<?> "line"
  operation <- operationParser P.<?> "operationParser"
  divisor <- P.string "  Test: divisible by " *> P.decimal <* P.endOfLine
  trueMonkeyId <- P.string "    If true: throw to monkey " *> P.decimal <* P.endOfLine
  falseMonkeyId <- P.string "    If false: throw to monkey " *> P.decimal
  P.endOfLine <* (P.endOfLine <|> P.endOfInput)

  return $ Monkey {
    monkeyId = MonkeyId monkeyId,
    startingItems = items,
    operation = operation,
    divisor = divisor,
    trueTarget = MonkeyId trueMonkeyId,
    falseTarget = MonkeyId falseMonkeyId
  }

  where
    operatorParser = ((*) <$ P.string " * ") <|> ((+) <$ P.string " + ")

    operandParser = (Old <$ "old") <|> (Operand <$> P.decimal)

    operationParser = do
      P.string "  Operation: new = "
      operand1 <- operandParser
      operator <- operatorParser
      operand2 <- operandParser
      P.endOfLine
      return (\x -> opToVal operand1 x `operator` opToVal operand2 x)

parse :: Text -> Either String Monkeys
parse = P.parseOnly (P.many1 monkeyParser)

-- TODO: it would be nice to use Reader to thread the worryLevel function through
runMonkey :: (Monkey -> Integer -> Integer) -> Monkey -> MonkeyStates -> MonkeyStates
runMonkey worryLevel monkey mStates = (updateMonkeyReceivers . updateMonkey) mStates
  where
    monkeyState = mStates M.! monkey.monkeyId
    targets = map
      (\item -> ((getTargetMonkey . worryLevel monkey) item, worryLevel monkey item))
      monkeyState.items

    getTargetMonkey x = if x `mod` monkey.divisor == 0
                           then monkey.trueTarget
                           else monkey.falseTarget

    updateMonkey =
      M.insert
        monkey.monkeyId
        (monkeyState {
          items = [],
          itemCounts = monkeyState.itemCounts + toInteger (length monkeyState.items)
        })

    updateMonkeyReceivers :: MonkeyStates -> MonkeyStates
    updateMonkeyReceivers _mStates =
      foldl'
        (\ms (targetId, itemsToGive) ->
            M.alter
              (\case
                Just state -> Just state { items = state.items <> [itemsToGive] }
                Nothing -> Nothing)
              targetId
              ms)
        _mStates
        targets

runRound :: (Monkey -> Integer -> Integer) -> Monkeys -> MonkeyStates -> MonkeyStates
runRound worryLevel monkeys mStates = foldl' (flip (runMonkey worryLevel)) mStates monkeys

monkeyOrdering :: MonkeyState -> MonkeyState -> Ordering
monkeyOrdering m1 m2
  | m1.itemCounts == m2.itemCounts = EQ
  | m1.itemCounts > m2.itemCounts = GT
  | otherwise = LT

initialState :: Monkeys -> MonkeyStates
initialState = M.fromList . map (\m -> (m.monkeyId, MonkeyState {items=m.startingItems, itemCounts=0}))

monkeyBusinessLvl :: MonkeyStates -> Integer
monkeyBusinessLvl = product . map itemCounts . take 2 . sortBy (flip monkeyOrdering) . M.elems

solve1 :: Monkeys -> Integer
solve1 monkeys = monkeyBusinessLvl finalStates
  where
    finalStates =
      nTimes 20 (runRound (\m item -> m.operation item `div` 3) monkeys) (initialState monkeys)

solve2 :: Monkeys -> Integer
solve2 ms = monkeyBusinessLvl finalStates
  where
    finalStates =
      nTimes
        10_000
        (runRound (\m item -> m.operation item `mod` commonMultiple) ms)
        (initialState ms)
      where
        commonMultiple = product . map (.divisor) $ ms

main :: IO ()
main = do
  monkeysE <- parse <$> TIO.readFile "data/day11.txt"

  putStrLn "part1:"
  print $ fmap solve1 monkeysE

  putStrLn "part2:"
  print $ fmap solve2 monkeysE
