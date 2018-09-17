import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve <$> replicateM n (getl $ wrds toInt) >>= mapM_ putStrLn
    main
    
solve :: [[Int]] -> [String]
solve = map mult
  where
    mult [x1,y1,z1,w1,x2,y2,z2,w2] = unwords . map show $ [(x1*x2-y1*y2-z1*z2-w1*w2),
                                                           (x1*y2+y1*x2+z1*w2-w1*z2),
                                                           (x1*z2-y1*w2+z1*x2+w1*y2),
                                                           (x1*w2+y1*z2-z1*y2+w1*x2)]

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
