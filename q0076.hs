import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  when (n /= (-1)) $ do
    mapM_ print $ solve n
    main

solve :: Int -> [Double]
solve n = (fromPolar . walk (n - 2)) (1.0, 0.0)

walk :: Int -> (Double,Double) -> (Double,Double)
walk n (r, a)
  | n == 0 = (nr, na)
  | otherwise = walk (n - 1) (nr, na)
  where
    nr = sqrt (r ^ 2 + 1.0)
    na = a + atan (1.0 / r)

fromPolar :: (Double,Double) -> [Double]
fromPolar (r,a) = [r * cos a, r * sin a]

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine
