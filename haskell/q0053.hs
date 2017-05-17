import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  n <- readLn :: IO Int
  when (n /= 0) $ do
    print $ solve n
    main
 
solve :: Int -> Int
solve x = sum $ take x pn
 
pn = 2:3:[x | x <- [5,7..], not (any (\y -> x `mod` y == 0) (takeWhile (\z -> z * z <= x) pn))]
