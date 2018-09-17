import Control.Applicative ((<$>))
import Control.Monad (unless, guard)

main :: IO ()
main = do
  [x, y, s] <- map read <$> words <$> getLine
  unless (x == 0 && y == 0 && s == 0) $ do
    print $ solve x y s
    main

solve :: Int -> Int -> Int -> Int
solve x y s = maximum $ do
  (a1, b1) <- ls
  (a2, b2) <- ls
  guard $ a1 + a2 == s
  return $ b1 + b2
  where ls = takeWhile (\(u,_) -> u < s) $ do
          a <- [1..]
          return (a * (100+x) `div` 100, a * (100+y) `div` 100)
