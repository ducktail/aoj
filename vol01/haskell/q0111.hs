import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  xs <- getc solve
  mapM_ putStrLn xs

solve :: String -> String
solve  = decode "" . encodes

encodec :: Char -> String
encodec c
  | c == 'A'  = "00000"
  | c == 'B'  = "00001"
  | c == 'C'  = "00010"
  | c == 'D'  = "00011"
  | c == 'E'  = "00100"
  | c == 'F'  = "00101"
  | c == 'G'  = "00110"
  | c == 'H'  = "00111"
  | c == 'I'  = "01000"
  | c == 'J'  = "01001"
  | c == 'K'  = "01010"
  | c == 'L'  = "01011"
  | c == 'M'  = "01100"
  | c == 'N'  = "01101"
  | c == 'O'  = "01110"
  | c == 'P'  = "01111"
  | c == 'Q'  = "10000"
  | c == 'R'  = "10001"
  | c == 'S'  = "10010"
  | c == 'T'  = "10011"
  | c == 'U'  = "10100"
  | c == 'V'  = "10101"
  | c == 'W'  = "10110"
  | c == 'X'  = "10111"
  | c == 'Y'  = "11000"
  | c == 'Z'  = "11001"
  | c == ' '  = "11010"
  | c == '.'  = "11011"
  | c == ','  = "11100"
  | c == '-'  = "11101"
  | c == '\'' = "11110"
  | c == '?'  = "11111"

encodes :: String -> String
encodes s = concatMap encodec s

decode :: String -> String -> String
decode acc s
  | take 3 s == "101" = decode (' ' : acc) (drop 3 s)
  | take 3 s == "110" = decode ('E' : acc) (drop 3 s)
  | take 3 s == "111" = decode ('P' : acc) (drop 3 s)
  | take 4 s == "0001" = decode ('D' : acc) (drop 4 s)
  | take 4 s == "0101" = decode ('C' : acc) (drop 4 s)
  | take 4 s == "0111" = decode ('I' : acc) (drop 4 s)
  | take 4 s == "0110" = decode ('K' : acc) (drop 4 s)
  | take 4 s == "1000" = decode ('R' : acc) (drop 4 s)
  | take 5 s == "00100" = decode ('L' : acc) (drop 5 s)
  | take 5 s == "00101" = decode ('O' : acc) (drop 5 s)
  | take 5 s == "00110" = decode ('S' : acc) (drop 5 s)
  | take 5 s == "00111" = decode ('T' : acc) (drop 5 s)
  | take 5 s == "01001" = decode ('F' : acc) (drop 5 s)
  | take 6 s == "000000" = decode ('\'' : acc) (drop 6 s)
  | take 6 s == "000001" = decode ('?' : acc) (drop 6 s)
  | take 6 s == "000010" = decode ('W' : acc) (drop 6 s)
  | take 6 s == "000011" = decode (',' : acc) (drop 6 s)
  | take 6 s == "010001" = decode ('.' : acc) (drop 6 s)
  | take 6 s == "010000" = decode ('H' : acc) (drop 6 s)
  | take 6 s == "100101" = decode ('A' : acc) (drop 6 s)
  | take 8 s == "10010000" = decode ('Z' : acc) (drop 8 s)
  | take 8 s == "10010001" = decode ('-' : acc) (drop 8 s)
  | take 8 s == "10010010" = decode ('X' : acc) (drop 8 s)
  | take 8 s == "10010011" = decode ('Y' : acc) (drop 8 s)
  | take 8 s == "10011000" = decode ('J' : acc) (drop 8 s)
  | take 8 s == "10011001" = decode ('M' : acc) (drop 8 s)
  | take 8 s == "10011010" = decode ('B' : acc) (drop 8 s)
  | take 8 s == "10011011" = decode ('G' : acc) (drop 8 s)
  | take 8 s == "10011100" = decode ('U' : acc) (drop 8 s)
  | take 8 s == "10011101" = decode ('V' : acc) (drop 8 s)
  | take 8 s == "10011110" = decode ('N' : acc) (drop 8 s)
  | take 8 s == "10011111" = decode ('Q' : acc) (drop 8 s)
  | otherwise = reverse acc

toInt :: String -> Int
toInt s = read s

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents

tests :: [Test]
tests = [
  "encodec1" ~: "10110" ~=? encodec 'W',
  "encodec2" ~: "11110" ~=? encodec '\'',
  "encodes1" ~: "000000000100010" ~=? encodes "ABC",
  "encodes2" ~: "1101111100111011111011111" ~=? encodes ".,-'?",
  "decode1"  ~: "PETER POTTER" ~=? decode "" "11111000111110100010111100101001110011111010000000",
  "solve1"   ~: "PETER POTTER" ~=? solve "?D-C'KOPUA",
  "solve2"   ~: "PETER PIPER PICKED A PECK OF PICKLED PEPPERS. A PECK OF PICKLED " ~=? solve "?D-C'?-C'-LMGZN?FNJKN- WEYN?P'QMRWLPZLKKTPOVRGDI",
  "solve3"   ~: "PEPPERS PETER PIPER PICKED. IF PETER PIPER PICKED A PECK OF PICKLED " ~=? solve "?P'QNPY?IXX?IXXK.BI -G?R'RPP'RPOVWDMW?SWUVG'-LCMGQ",
  "solve4"   ~: "PEPPERS, WHERE'S THE PECK OF PICKLED PEPPERS PETER PIPER PICKED?" ~=? solve "?P'QMDUEQ GADKOQ ?SWUVG'-LCMG?X?IGX,PUL.?UL.VNQQI"
  ]
