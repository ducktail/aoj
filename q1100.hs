import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = main' 1
main' c  = do
  n <- getl toInt
  unless (n == 0) $ do
    solve <$> replicateM n (getl (toTpl . wrds toDbl)) >>= printAnswer c
    getLine
    main' (c+1)

solve :: [(Double, Double)] -> Double
solve (x:xs) = f 0 xs
  where
    f s [p] = abs s * 0.5
    f s (p1:p2:ps) = f (s + cross x p1 p2) (p2:ps)

printAnswer :: Int -> Double -> IO ()
printAnswer c a = putStrLn $ show c ++ " " ++ show a

cross :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
cross (px1, py1) (px2, py2) (px3, py3) = (px2 - px1) * (py3 - py1) - (py2 - py1) * (px3 - px1)

toTpl :: [a] -> (a,a)
toTpl [x,y] = (x,y)
  
toInt :: String -> Int
toInt = read

toDbl :: String -> Double
toDbl = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
