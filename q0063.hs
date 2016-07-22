import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  ss <- getc id
  print $ solve ss

solve :: [String] -> Int
solve = length . filter isPalindrome

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
