import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

main :: IO ()
main = do
  n <- getl toInt
  xs <- rgetl n solve
  mapM_ putStrLn xs

solve :: String -> String
solve = intercalate "Hoshina" . splitOn "Hoshino"

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine
