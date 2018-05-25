import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  getc (map (solve . wrds toDbl)) >>= mapM_ print

solve :: [Double] -> Double
solve [a, l, x] = abecd a l x
  where
    abc a l = sqrt (4 * l ^ 2 - a ^ 2) * a / 4
    adc l x = sqrt (2 * l * x + x ^ 2) * l / 4
    abecd a l x = abc a l + 2 * (adc l x)
    
toDbl :: String -> Double
toDbl = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getc :: ([String] -> a) -> IO a
getc f = f . lines <$> getContents
