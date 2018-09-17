import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

main = do
  xs <- input ( toTpl . splitOn "," )
  putStrLn $ solve xs
 
solve :: [(String,String)] -> String
solve xs = foldl exchange "A" xs
 
exchange :: String -> (String, String) -> String
exchange s (x, y)
  | s == x = y
  | s == y = x
  | otherwise = s
 
input :: (String -> a) -> IO [a]
input f = map f . lines <$> getContents 
 
toTpl :: [String] -> (String,String)
toTpl [x,y] = (x,y)
