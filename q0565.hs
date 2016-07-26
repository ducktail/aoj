import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> replicateM 5 (getl toInt) >>= print

solve :: [Int] -> Int
solve [p1,p2,p3,j1,j2] = (p1 `min` p2 `min` p3) + (min j1 j2) - 50

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO agetl f = f <$> getLine
