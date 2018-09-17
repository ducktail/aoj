import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> replicateM 6 (getl toInt) >>= print

solve :: [Int] -> Int
solve [a,b,c,d,e,f] = (sum . tail . sort) [a,b,c,d] + max e f

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
