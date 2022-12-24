module Solutions.Day13 (main) where
import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.List            (intercalate)
import           Data.Maybe           (catMaybes)
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Lib.Common           (mapWithIndex)

data PacketData = PacketInt Int | PacketList [PacketData] deriving (Eq)

instance Show PacketData where
  show (PacketInt x)  = show x
  show (PacketList x) = "[" ++ intercalate "," (map show x) ++ "]"

type Packet = [PacketData]

parse :: Text -> Either String [(Packet, Packet)]
parse = P.parseOnly $ pairParser `P.sepBy1` "\n\n"
  where
    packetParser :: Parser Packet
    packetParser = do
      P.char '['
      packetData <- (PacketInt <$> P.decimal <|> PacketList <$> packetParser) `P.sepBy` ","
      P.char ']'
      return packetData

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
pairIsInRightOrder (p1:restOfP1, p2:restOfP2) =
  case comparePacketData p1 p2 of
    Nothing     -> pairIsInRightOrder (restOfP1, restOfP2)
    Just result -> result
pairIsInRightOrder ([], _) = True
pairIsInRightOrder (_, []) = False

part1 :: [(Packet, Packet)] -> Int
part1 = sum . catMaybes . mapWithIndex pairOrder
  where
    pairOrder idx pair = if pairIsInRightOrder pair then Just (idx + 1) else Nothing

main :: IO ()
main = do
  input <- parse <$> TIO.readFile "data/day13.txt"
  putStrLn "Part 1: "
  print $ part1 <$> input
