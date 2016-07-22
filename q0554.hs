import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> replicateM 4 (getl toInt) >>= mapM_ print

solve :: [Int] -> [Int]
solve = f . sum
  where
    f x = [x `div` 60, x `mod` 60]

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
