import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n f >>= mapM_ print
    putStrLn "#"
    main
  where
    f = g <$> words <$> getLine
    g [l, p, a, b, c, d, e, f, s, m] = Crop l (read p) (read a) (read b) (read c) (read d) (read e) (read f) (read s) (read m)

solve :: [Crop] -> [Crop]
solve = sortBy cmp
  where
    g cr = let prft = m cr * f cr * s cr - p cr
               prd = a cr + b cr + c cr + m cr * (d cr + e cr)
           in (prft, prd)
    cmp cr1 cr2 = let (w, x) = g cr1
                      (y, z) = g cr2
                  in if x * y == w * z
                     then compare (l cr1) (l cr2)
                     else compare (x * y) (w * z)
      
data Crop = Crop {l::String, p::Int, a::Int, b::Int, c::Int, d::Int, e::Int, f::Int, s::Int, m::Int}

instance Show Crop where
  show cr = l cr
