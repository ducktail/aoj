import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Bool (bool)

main :: IO ()
main = do
  n <- readLn
  map solve <$> replicateM n f >>= mapM_ putStrLn
  where
    f = head <$> B.words <$> B.getLine

solve :: ByteString -> String
solve bs = bool "No" "Yes" $ f 0 bs || f 1 bs
  where
    f lr bs = case g bs of
      Just (stp, bs') | isNatural lr stp -> f (1 - lr) bs'
      Just _ -> False
      Nothing -> True
    g bs = do
      (c1, bs') <- B.uncons bs
      (c2, _) <- B.uncons bs'
      return ((c1, c2), bs')

isNatural :: Int -> (Char, Char) -> Bool
isNatural lr stp
  | lr == 0   = stp `elem` [('U','R'),('U','D'),('L','U'),('L','R'),('L','D'),('D','U'),('D','R')]
  | otherwise = stp `elem` [('U','L'),('U','D'),('R','U'),('R','L'),('R','D'),('D','U'),('D','L')]
