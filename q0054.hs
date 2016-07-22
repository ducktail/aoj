import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- input read :: IO [Double]
  mapM_ print $ solve xs
 
solve :: [Double] -> [Double]
solve xs = map f xs
  where f x = x * 7.814814814814814
 
input :: (String -> a) -> IO [a]
input f = map f . lines <$> getContents 
