import Control.Applicative
import Control.Monad
import Numeric ( readHex )
import Data.Bits ((.&.), shiftR, testBit)
import Text.Printf

main :: IO ()
main = do
  n <- readLn
  map solve <$> replicateM n getLine >>= mapM_ putStrLn

solve :: String -> String
solve s = sgn ++ it ++ "." ++ f (n .&. 0x7f)
  where
    ((n, _) : _) = readHex s :: [(Int, String)]
    sgn = if testBit n 31 then "-" else ""
    it = show $ shiftR (n .&. 0x7fffff80) 7
    f 0 = "0"
    f ex = reverse $ dropWhile (== '0') $ reverse s 
      where
        b6 = if testBit ex 6 then 5000000 else 0 :: Int
        b5 = if testBit ex 5 then 2500000 else 0 :: Int
        b4 = if testBit ex 4 then 1250000 else 0 :: Int
        b3 = if testBit ex 3 then  625000 else 0 :: Int
        b2 = if testBit ex 2 then  312500 else 0 :: Int
        b1 = if testBit ex 1 then  156250 else 0 :: Int
        b0 = if testBit ex 0 then   78125 else 0 :: Int
        s = printf "%07d" (b6 + b5 + b4 + b3 + b2 + b1 + b0) :: String
