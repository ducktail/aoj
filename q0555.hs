import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  s <- getLine
  n <- getl toInt
  solve s <$> replicateM n getLine >>= print

solve :: String -> [String] -> Int
solve s = sum . map f
  where
    f x
      | s `isInfixOf` (x ++ x) = 1
      | otherwise = 0

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
