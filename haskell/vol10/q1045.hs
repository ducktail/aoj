import Control.Applicative ((<$>))
import Control.Monad (unless)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> map read <$> words <$> getLine >>= print
    main

solve :: [Int] -> Int
solve = f 0 0
  where f a b [] = abs (a - b)
        f a b (x:xs) = min (f (a+x) b xs) (f a (b+x) xs)
