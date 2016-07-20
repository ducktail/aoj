import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import qualified Data.Set as S

main :: IO ()
main = do
  m <- getl toInt
  unless (m == 0) $ do
    xs <- replicateM m (getl $ wrds toInt)
    n <- getl toInt
    unwords . map show . solve xs <$> replicateM n (getl $ wrds toInt) >>= putStrLn
    main

solve :: [[Int]] -> [[Int]] -> [Int]
solve xs ys = maybe [] id $ find f ls
  where
    db = S.fromList ys
    ls = map (\cs -> zipWith (-) cs (head xs)) ys
    f z = all (\c -> S.member c db) $ map (zipWith (+) z) xs

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
