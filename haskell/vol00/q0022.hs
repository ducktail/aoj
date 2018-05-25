import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  n <- readLn :: IO Int
  when (n /= 0) $ do
    xs <- input n read :: IO [Int]
    print $ solve xs
    main
     
solve :: [Int] -> Int
solve (x:xs) = mss [x] xs
 
mss :: [Int] -> [Int] -> Int
mss ls [] = maximum ls
mss ls (x:xs) | w > x = mss (w:ls) xs
              | otherwise = mss (x:ls) xs
  where
    w = head ls + x
 
input :: Int -> (String -> a) -> IO [a]
input n f = map f <$> replicateM n getLine
