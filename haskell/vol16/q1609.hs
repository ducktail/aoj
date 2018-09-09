import Control.Applicative
import Control.Monad
import Data.List
import Data.Function (on, fix)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> f >>= putStrLn
    main
  where
    f = map head <$> words <$> getLine

solve :: Int -> [Char] -> String
solve n vs = f 1 (zip ['A' .. 'Z'] (repeat 0)) vs
  where
    f i sb [] = "TIE"
    f i sb (c:cs) = let (as, (x,y):bs) = break ((==c).fst) sb
                        sb'@((w,p1):(_,p2):_) = sortBy (flip compare `on` snd) $ as ++ (x, y+1) : bs
                    in if p1 > p2 + n - i
                       then w : ' ' : show i
                       else f (i + 1) sb' cs
