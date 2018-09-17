import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  getc solve >>= mapM_ print

solve :: [String] -> [Int]
solve [] = []
solve (x:xs:ys) = f : solve ys
  where
    n = toInt x
    ps = sort $ wrds toInt xs
    f = snd $ foldl (\(a,b) p -> (a+p,b+a+p)) (0,0) ps

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getc :: ([String] -> a) -> IO a
getc f = f . lines <$> getContents
