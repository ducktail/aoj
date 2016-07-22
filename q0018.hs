import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- inp $ split toInt
  putStrLn . unwords . map show $ sortBy (\x y -> compare y x) xs

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

inp :: (String -> a) -> IO a
inp f = f <$> getLine
