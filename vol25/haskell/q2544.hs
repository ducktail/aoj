import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [n, q] <- f
  solve q <$> replicateM n g >>= putStrLn
  where
    f = map read <$> words <$> getLine
    g = (\[yr, nm] -> (read yr, nm)) <$> words <$> getLine

solve :: Int -> [(Int, String)] -> String
solve q = snd . last . takeWhile ((q >=) . fst) . ((1, "kogakubu10gokan"):)
