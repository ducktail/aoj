import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  solve <$> replicateM n (getl $ wrds toInt) >>= mapM_ putStrLn

solve :: [[Int]] -> [String]
solve = map f
  where
    f [r,t]
      | rr == 0 && tr == 0 = show $ tq*5+rq
      | rr == 0 = show (tq*5+rq) ++ " " ++ show ((tq+1)*5+rq)
      | tr == 0 = show (tq*5+rq) ++ " " ++ show (tq*5+rq+1)
      | otherwise = (unwords . map show) [tq*5+rq,tq*5+rq+1,(tq+1)*5+rq,(tq+1)*5+rq+1]
      where
        (rq,rr) = r `divMod` 100
        (tq,tr) = t `divMod` 30

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
