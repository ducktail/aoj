import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.Map (Map)
import qualified Data.Map as M

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    tbl <- M.fromList <$> map f <$> replicateM n (words <$> getLine)
    m <- readLn
    solve tbl <$> replicateM m (head <$> getLine) >>= putStrLn
    main
  where f [a, b] = (head a, head b)

solve :: Map Char Char -> String -> String
solve tbl = map f
  where f c = maybe c id $ M.lookup c tbl
