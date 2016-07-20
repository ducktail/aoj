import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> replicateM 10 (getl toInt) <*> replicateM 10 (getl toInt) >>= putStrLn . unwords . map show

solve :: [Int] -> [Int] -> [Int]
solve xs ys = map f [xs, ys]
  where
    f = sum . take 3 . sortBy (flip compare)

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
