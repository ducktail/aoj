import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> replicateM 5 (getl toInt) >>= print

solve :: [Int] -> Int
solve = flip div 5 . sum . map f
  where
    f x
      | x < 40 = 40
      | otherwise = x

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
