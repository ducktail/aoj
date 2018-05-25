import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n,m] <- getl $ wrds toInt
  unless (n == 0 && m == 0) $ do
    solve m <$> getl (wrds toInt) >>= print
    main
  
solve :: Int -> [Int] -> Int
solve m = sum . map snd . filter (\x -> fst x `mod` m /= 0) . zip [1..] . sortBy (flip compare)

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
