import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Text.Printf

main :: IO ()
main = do
  xs <- getc $ solve . toInt
  mapM_ printId xs

solve :: Int -> Int
solve x
  | r == 0 = 39
  | otherwise = r
  where
    r = x `mod` 39

printId :: Int -> IO ()
printId = printf "3C%02d\n"

toInt :: String -> Int
toInt s = read s

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
