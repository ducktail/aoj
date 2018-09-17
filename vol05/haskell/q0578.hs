import Control.Applicative ((<$>))
import Control.Monad (replicateM, foldM, guard)
import Data.List (foldl', findIndices)

main :: IO ()
main = do
  n <- readLn
  nbd <- getLine
  solve nbd <$> replicateM n getLine >>= print

solve :: String -> [String] -> Int
solve nbd = foldl' f 0
  where f ct obd = case ixc of
                    (_:_) -> ct + 1
                    _ -> ct
          where ixs = map (\c -> findIndices (==c) obd) nbd
                ixc = foldM (\xs ys -> do
                                y <- ys
                                case xs of
                                 [] -> return [y]
                                 [x] -> do
                                   guard $ y > x
                                   return [y,x]
                                 (x1:x2:rs) -> do
                                   guard $ y > x1 && (y - x1) == (x1 - x2)
                                   return $ y:xs
                            ) [] ixs
