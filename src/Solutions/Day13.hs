module Solutions.Day13 (main) where
import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.List            (elemIndex, intercalate, sort)
import           Data.Maybe           (catMaybes)
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Lib.Common           (Solution (Solution), mapWithIndex, runSolution)

data PacketData = PacketInt Int | PacketList [PacketData] deriving (Eq)

instance Show PacketData where
  show (PacketInt x)  = show x
  show (PacketList x) = "[" ++ intercalate "," (map show x) ++ "]"

newtype Packet = Packet { unpacket :: [PacketData] } deriving (Show, Eq)

instance Ord Packet where
  (<=) = curry pairIsInRightOrder

parse :: Text -> Either String [(Packet, Packet)]
parse = P.parseOnly $ pairParser `P.sepBy1` "\n\n"
  where
    packetParser :: Parser Packet
    packetParser = do
      P.char '['
      packetData <- (
        PacketInt <$> P.decimal
        <|> PacketList . unpacket <$> packetParser
                    ) `P.sepBy` ","
      P.char ']'
      return (Packet packetData)

    pairParser :: Parser (Packet, Packet)
    pairParser = do
      packet1 <- packetParser
      P.endOfLine
      packet2 <- packetParser
      return (packet1, packet2)

comparePacketData :: PacketData -> PacketData -> Maybe Bool
-- Basic base for comparing two integer values
comparePacketData (PacketInt p1) (PacketInt p2)
  | p1 < p2 = Just True
  | p1 > p2 = Just False
  | otherwise = Nothing
-- If only one item is an integer, we wrap it in a list and do the comparison again
comparePacketData p1@(PacketInt _) plist2@(PacketList _) =
  comparePacketData (PacketList [p1]) plist2
comparePacketData plist1@(PacketList _) p2@(PacketInt _) =
  comparePacketData plist1 (PacketList [p2])
-- Cases for handling two lists
comparePacketData (PacketList (p1:restOfP1)) (PacketList (p2:restOfP2)) =
  case comparePacketData p1 p2 of
    Nothing -> comparePacketData (PacketList restOfP1) (PacketList restOfP2)
    result  -> result
comparePacketData (PacketList []) (PacketList (_:_)) = Just True
comparePacketData (PacketList (_:_)) (PacketList [])= Just False
comparePacketData (PacketList []) (PacketList [])= Nothing

pairIsInRightOrder :: (Packet, Packet) -> Bool
pairIsInRightOrder (Packet (p1:restOfP1), Packet (p2:restOfP2)) =
  case comparePacketData p1 p2 of
    Nothing     -> pairIsInRightOrder (Packet restOfP1, Packet restOfP2)
    Just result -> result
pairIsInRightOrder (Packet [], _) = True
pairIsInRightOrder (_, Packet []) = False

part1 :: [(Packet, Packet)] -> Int
part1 = sum . catMaybes . mapWithIndex pairOrder
  where
    pairOrder idx (packet1, packet2) = if packet1 < packet2 then Just (idx + 1) else Nothing

part2 :: [(Packet, Packet)] -> Int
part2 packets = product $ findDividerIdxs $ sort (divider2 : divider6 : flattenedPackets)
  where
    flattenedPackets = concatMap (\(p1, p2) -> [p1, p2]) packets
    divider2 = Packet [PacketList [PacketInt 2]]
    divider6 = Packet [PacketList [PacketInt 6]]
    findDividerIdxs x = map (+1) $ catMaybes [elemIndex divider2 x, elemIndex divider6 x]

solution :: Solution [(Packet, Packet)] Int
solution = Solution parse part1 part2

main :: IO ()
main = runSolution "data/day13.txt" solution
