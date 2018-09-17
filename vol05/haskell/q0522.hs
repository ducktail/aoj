import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  getc solve >>= mapM_ (mapM_ print)

solve :: [String] -> [[Int]]
solve = map (f 0 0)
  where
    f j i ss | length ss < 3 = [j,i]
             | s == "JOI" = f (j+1) i (drop 2 ss)
             | s == "IOI" = f j (i+1) (drop 2 ss)
             | otherwise = f j i (tail ss)
      where
        s = take 3 ss

getc :: ([String] -> a) -> IO a
getc f = f . lines <$> getContents
