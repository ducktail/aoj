import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [m, _] <- f
  maximum <$> replicateM m (sum <$> f) >>= print
  where
    f = map read <$> words <$> getLine
