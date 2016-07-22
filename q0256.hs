import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  sum <$> replicateM 10 (getl toInt) >>= print

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
