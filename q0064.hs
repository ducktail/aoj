import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Char (isDigit, digitToInt)

main :: IO ()
main = do
  xs <- getc id
  print $ solve xs

solve :: [String] -> Int
solve = sum . map secretNumber

secretNumber :: String -> Int
secretNumber = uncurry (+) . foldl f (0,0)
  where
    f :: (Int, Int) -> Char -> (Int, Int)
    f (s, t) c 
      | isDigit c = (s, 10 * t + digitToInt c)
      | otherwise = (s + t, 0)

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
