import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (group)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> getLine >>= putStrLn
    main

solve :: Int -> String -> String
solve n s = iterate f s !! n
  where f = concat . map g . group
        g x = show (length x) ++ [head x]
