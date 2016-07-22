import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = getc code >>= mapM_ putStrLn

code :: String -> String
code = fromBin . split5 . toBin

toBin :: String -> String
toBin = concatMap f
  where
    f c
      | c == ' ' = "101"
      | c == '\'' = "000000"
      | c == ',' = "000011"
      | c == '-' = "10010001"
      | c == '.' = "010001"
      | c == '?' = "000001"
      | c == 'A' = "100101"
      | c == 'B' = "10011010"
      | c == 'C' = "0101"
      | c == 'D' = "0001"
      | c == 'E' = "110"
      | c == 'F' = "01001"
      | c == 'G' = "10011011"
      | c == 'H' = "010000"
      | c == 'I' = "0111"
      | c == 'J' = "10011000"
      | c == 'K' = "0110"
      | c == 'L' = "00100"
      | c == 'M' = "10011001"
      | c == 'N' = "10011110"
      | c == 'O' = "00101"
      | c == 'P' = "111"
      | c == 'Q' = "10011111"
      | c == 'R' = "1000"
      | c == 'S' = "00110"
      | c == 'T' = "00111"
      | c == 'U' = "10011100"
      | c == 'V' = "10011101"
      | c == 'W' = "000010"
      | c == 'X' = "10010010"
      | c == 'Y' = "10010011"
      | c == 'Z' = "10010000"

split5 :: String -> [String]
split5 ss
  | b == [] = [c]
  | otherwise = a : split5 b
  where
    (a,b) = splitAt 5 ss
    c = take 5 $ a ++ "0000"

fromBin :: [String] -> String
fromBin = map f
  where
    f s
      | s == "00000" = 'A'
      | s == "00001" = 'B'
      | s == "00010" = 'C'
      | s == "00011" = 'D'
      | s == "00100" = 'E'
      | s == "00101" = 'F'
      | s == "00110" = 'G'
      | s == "00111" = 'H'
      | s == "01000" = 'I'
      | s == "01001" = 'J'
      | s == "01010" = 'K'
      | s == "01011" = 'L'
      | s == "01100" = 'M'
      | s == "01101" = 'N'
      | s == "01110" = 'O'
      | s == "01111" = 'P'
      | s == "10000" = 'Q'
      | s == "10001" = 'R'
      | s == "10010" = 'S'
      | s == "10011" = 'T'
      | s == "10100" = 'U'
      | s == "10101" = 'V'
      | s == "10110" = 'W'
      | s == "10111" = 'X'
      | s == "11000" = 'Y'
      | s == "11001" = 'Z'
      | s == "11010" = ' '
      | s == "11011" = '.'
      | s == "11100" = ','
      | s == "11101" = '-'
      | s == "11110" = '\''
      | s == "11111" = '?'

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
