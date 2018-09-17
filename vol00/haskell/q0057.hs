import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- getc toInt
  mapM_ print $ solve xs

solve :: [Int] -> [Int]
solve = map numOfArea

numOfArea :: Int -> Int
numOfArea 1 = 2
numOfArea n = n + numOfArea (n - 1)

toInt :: String -> Int
toInt s = read s

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
