import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (sort, findIndex)

main :: IO ()
main = do
  [h, w] <- map read <$> words <$> getLine
  unless (h == 0 && w == 0) $ do
    putStrLn $ solve db h w
    main
  where db = sort $ do
          w <- [2..150]
          h <- [1..w-1]
          return (h^2 + w^2, h, w)
  
solve :: [(Int, Int, Int)] -> Int -> Int -> String
solve db h w = let Just i = findIndex (\(_, a, b) -> a == h && b == w) db
               in f $ db !! (i+1)
  where f (_, a, b) = unwords $ map show $ [a, b]
