import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  x <- readLn :: IO Int
  when ( x /= 0 ) $ do
    print $ solve x
    main
 
solve :: Int -> Int
solve x = (sum . map (f 0)) ls
  where ls = [5,10..x]
        f :: Int -> Int -> Int
        f ac y | y `mod` 5 == 0 = f (ac+1) (y `div` 5)
               | otherwise = ac
