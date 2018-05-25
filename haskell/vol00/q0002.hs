import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- getContents
  mapM_ print . map (length . show . toSum) $ lines xs
 
toSum :: String -> Int
toSum = sum . map read . words
