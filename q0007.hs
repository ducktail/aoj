import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  n <- read <$> getLine :: IO Int
  print $ solve n
 
solve :: Int -> Int
solve x = (iterate ((*1000) . ceiling . (*0.00105) . fromIntegral) 100000) !! x
