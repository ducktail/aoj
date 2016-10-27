import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl read
  unless (n == 0) $ do
    solve n <$> replicateM n (getl read) >>= print
    main

solve :: Int -> [Int] -> Int
solve n xs =  (sum . init . tail . sort) xs `div` (n-2)

getl :: (String -> a) -> IO a
getl f = f <$> getLine
