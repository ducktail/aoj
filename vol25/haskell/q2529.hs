import Control.Applicative
import Control.Monad
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Char (ord)

main :: IO ()
main = getLine >>= solve

solve :: String -> IO ()
solve s = do
  putStr "+++>+++++>+++++++>+>+++>+++++>+++++++>+>+++>+++++>>++++++++[-<<+++++++++++<++++++++++<+++++++++<+++++++<++++++<+++++<++++<++<+<[++++>]>]<<<<<<"
  v <- VM.replicate 13 0 :: IO (IOVector Int)
  VM.write v 3 35
  VM.write v 4 45
  VM.write v 5 55
  VM.write v 6 65
  VM.write v 7 75
  VM.write v 8 85
  VM.write v 9 95
  VM.write v 10 105
  VM.write v 11 115
  VM.write v 12 125
  foldM_ (f v) 8 cs
  putStrLn ""
  where
    cs = map ord s
    f v bi c = do
      let i = c `div` 10
      if bi > i
        then putStr (replicate (bi - i) '<')
        else putStr (replicate (i - bi) '>')
      d <- VM.read v i
      if d > c
        then putStr (replicate (d - c) '-')
        else putStr (replicate (c - d) '+')
      putStr "."
      VM.write v i c
      return i
