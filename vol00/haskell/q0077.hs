wimport Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Char (digitToInt)

main :: IO ()
main = do
  mapM_ putStrLn =<< getc solve

solve :: String -> String
solve [] = []
solve [s1] = [s1]
solve [s1,s2] = [s1,s2]
solve (s1:s2:s3:ss)
  | s1 == '@' = replicate (digitToInt s2) s3 ++ solve ss
  | otherwise = s1 : solve (s2:s3:ss)

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
