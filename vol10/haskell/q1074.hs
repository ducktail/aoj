import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.List (foldl')
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> replicateM n (f <$> words <$> getLine) >>= putStrLn
    main
  where f (s:_:ls) = (s, map read ls)

solve :: Int -> [(String, [Int])] -> String
solve n xs = f . minimum . map (\(s, ts) -> (count ts, s)) $ xs
  where ph = foldl' (\h x -> V.accum (+) h [(x,1)]) (V.replicate 31 0) (concatMap snd xs) :: Vector Int
        count = sum . map (\t -> n + 1 - ph ! t)
        f (t, s) = unwords [show t, s]
