import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Control.Arrow

main :: IO ()
main = do
  getc solve >>= mapM_ putStrLn

solve :: [String] -> [String]
solve = map f
  where
    f :: String -> String
    f = concatMap g . map (head &&& length) . group
    g :: (Char,Int) -> String
    g (c,n)
      | c == '0' = replicate (n-1) ' '
      | c == '1' = ["'", ",", ".", "!", "?"] !! (n-1)
      | c == '2' = ["a", "b", "c", "A", "B", "C"] !! (n-1)
      | c == '3' = ["d", "e", "f", "D", "E", "F"] !! (n-1)
      | c == '4' = ["g", "h", "i", "G", "H", "I"] !! (n-1)
      | c == '5' = ["j", "k", "l", "J", "K", "L"] !! (n-1)
      | c == '6' = ["m", "n", "o", "M", "N", "O"] !! (n-1)
      | c == '7' = ["p", "q", "r", "s", "P", "Q", "R", "S"] !! (n-1)
      | c == '8' = ["t", "u", "v", "T", "U", "V"] !! (n-1)
      | c == '9' = ["w", "x", "y", "z", "W", "X", "Y", "Z"] !! (n-1)
      | otherwise = ""
        
getc :: ([String] -> a) -> IO a
getc f = f . lines <$> getContents
