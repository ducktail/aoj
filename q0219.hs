import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Array.Unboxed

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve <$> replicateM n (getl toInt) >>= mapM_ putStrLn
    main

solve :: [Int] -> [String]
solve = map f . elems . accum (+) iar . map g
  where
    iar = listArray (0,9) (repeat 0) :: UArray Int Int
    f x = if x == 0 then "-" else replicate x '*'
    g x = (x,1)
      
toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
