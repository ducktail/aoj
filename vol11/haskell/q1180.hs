import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (unfoldr, sort)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

main :: IO ()
main = do
  [a, l] <- map read <$> words <$> getLine
  unless (a == 0 && l == 0) $ do
    putStrLn $ solve a l
    main

solve :: Int -> Int -> String
solve a l = f 1 (IM.singleton a 0) a
  where f i mp x = let ys = sort $ toL x l
                       mx = frL ys
                       mn = frL $ reverse ys
                       z = mx - mn
                   in case IM.lookup z mp of
                       Just j -> unwords $ map show $ [j, z, i-j]
                       Nothing -> f (i+1) (IM.insert z i mp) z

toL x l = unfoldr f (x, l)
  where f (x, l) | l == 0 = Nothing
                 | otherwise = let (q, r) = divMod x 10
                               in Just (r, (q, l-1))

frL = foldr f 0
  where f x a = 10 * a + x
