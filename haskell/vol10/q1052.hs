import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless, guard, foldM)
import Data.List (sort)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> sort <$> replicateM n (f <$> map read <$> words <$> getLine) >>= putStrLn
    main
  where f [a, b] = (b, a)
  
solve :: [(Int,Int)] -> String
solve = maybe "No" (const "Yes") . foldM f 0
  where f cw (t, w) = do
          guard $ cw + w <= t
          return $ cw + w
