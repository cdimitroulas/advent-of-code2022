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
import           Text.Show.Functions  ()

data Operand = Old | Operand Integer

opToVal :: Operand -> Integer -> Integer
opToVal (Operand x) _ = x
opToVal Old x         = x

newtype MonkeyId = MonkeyId Integer deriving (Show, Eq, Ord)

data Monkey = Monkey
                { monkeyId    :: !MonkeyId
                , items       :: ![Integer]
                , operation   :: !(Integer -> Integer)
                , divisor     :: !Integer
                , trueTarget  :: !MonkeyId
                , falseTarget :: !MonkeyId
                , itemCounts  :: !Integer
                }
  deriving (Show)

instance Eq Monkey where
  (==) a b = a.monkeyId == b.monkeyId

instance Ord Monkey where
  (<=) a b = a.monkeyId <= b.monkeyId

type Monkeys = Map MonkeyId Monkey

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
    items = items,
    operation = operation,
    divisor = divisor,
    trueTarget = MonkeyId trueMonkeyId,
    falseTarget = MonkeyId falseMonkeyId,
    itemCounts = 0
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
parse = fmap toMap . P.parseOnly (P.many1 monkeyParser)
  where
    toMap = M.fromList . map (\x -> (x.monkeyId, x))

-- TODO: it would be nice to use Reader to thread the worryLevel function through
runMonkey :: (Monkey -> Integer -> Integer) -> MonkeyId -> Monkeys -> Monkeys
runMonkey worryLevel mId monkeys = (updateMonkeyReceivers . updateMonkey) monkeys
  where
    monkey = monkeys M.! mId
    targets = map
      (\item -> ((getTargetMonkey . worryLevel monkey) item, worryLevel monkey item))
      monkey.items

    getTargetMonkey x = if x `mod` monkey.divisor == 0
                           then monkey.trueTarget
                           else monkey.falseTarget

    updateMonkey =
      M.insert
        mId
        (monkey { items = [], itemCounts = monkey.itemCounts + toInteger (length monkey.items) })

    updateMonkeyReceivers :: Monkeys -> Monkeys
    updateMonkeyReceivers _monkeys =
      foldl'
        (\ms (targetId, itemsToGive) ->
            M.alter
              (\case
                Just mkey -> Just mkey { items = mkey.items <> [itemsToGive] }
                Nothing -> Nothing)
              targetId
              ms)
        _monkeys
        targets

runRounds :: Integer -> Integer -> (Monkey -> Integer -> Integer) -> Monkeys -> Monkeys
runRounds maxRound currentRound worryLevel monkeys
  | maxRound + 1 == currentRound = monkeys
  | otherwise = runRounds maxRound (currentRound + 1) worryLevel newMonkeys
  where
    newMonkeys = foldl' (flip (runMonkey worryLevel)) monkeys (M.keys monkeys)

monkeyOrdering :: Monkey -> Monkey -> Ordering
monkeyOrdering m1 m2
  | m1.itemCounts == m2.itemCounts = EQ
  | m1.itemCounts > m2.itemCounts = GT
  | otherwise = LT

solve1 :: Monkeys -> Integer
solve1 = product . map itemCounts . take 2 . sortBy (flip monkeyOrdering)
  . M.elems
  . runRounds 20 1 (\m item -> m.operation item `div` 3)

solve2 :: Monkeys -> Integer
solve2 ms = product . map itemCounts . take 2 . sortBy (flip monkeyOrdering)
  . M.elems
  . runRounds 10_000 1 (\m item -> m.operation item `mod` commonMultiple)
  $ ms
      where
        commonMultiple = product . map (.divisor) $ M.elems ms

main :: IO ()
main = do
  monkeysE <- parse <$> TIO.readFile "data/day11.txt"

  putStrLn "part1:"
  print $ fmap solve1 monkeysE

  putStrLn "part2:"
  print $ fmap solve2 monkeysE
