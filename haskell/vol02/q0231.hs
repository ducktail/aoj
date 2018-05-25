import Control.Applicative ((<$>))
import Control.Monad (unless, foldM)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n >>= putStrLn
    main

solve :: Int -> IO String
solve n = do
  im <- foldM (\im _ -> do
                  [m, a, b] <- map read <$> words <$> getLine
                  let tm = IM.insertWith (+) a m im
                  return $ IM.insertWith (+) b (-m) tm
                  ) IM.empty [1..n]
  return . maybe "NG" (const "OK") . IM.foldl f (Just 0) $ im
  where f a b = do
          x <- a
          if x + b > 150 then Nothing else return (x + b)
