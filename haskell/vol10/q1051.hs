import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (foldl')

main :: IO ()
main = do
  n <- readLn
  unless (n==0) $ do
    solve <$> map read <$> words <$> getLine >>= putStrLn
    main

solve :: [Int] -> String
solve (x:xs) = unwords $ g $ foldl' f ((x,x),[]) xs
  where f ((a,b),ys) z | b+1 == z = ((a,z),ys)
                       | otherwise = ((z,z),(a,b):ys)
        g (x,y) = map h $ reverse (x:y)
        h (x, y) | x == y = show x
                 | otherwise = show x ++ "-" ++ show y
