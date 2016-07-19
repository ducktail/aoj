import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> replicateM 28 (getl toInt) >>= mapM_ print

solve :: [Int] -> [Int]
solve xs = sort $ [1..30] \\ xs

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
