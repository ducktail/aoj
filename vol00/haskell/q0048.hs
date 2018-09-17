import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- inpi toDbl
  mapM_ putStrLn $ solve xs

solve :: [Double] -> [String]
solve = map classify

classify :: Double -> String
classify s | s <= 48.0 = "light fly"
           | s <= 51.0 = "fly"
           | s <= 54.0 = "bantam"
           | s <= 57.0 = "feather"
           | s <= 60.0 = "light"
           | s <= 64.0 = "light welter"
           | s <= 69.0 = "welter"
           | s <= 75.0 = "light middle"
           | s <= 81.0 = "middle"
           | s <= 91.0 = "light heavy"
           | otherwise = "heavy"

toDbl :: String -> Double
toDbl s = read s

inpi :: (String -> a) -> IO [a]
inpi f = map f . lines <$> getContents
