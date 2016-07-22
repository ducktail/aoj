import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> getl (wrds toInt) >>= putStrLn

solve :: [Int] -> String
solve [1,1,0] = "Open"
solve [0,0,1] = "Open"
solve _ = "Close"

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
