import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  s1 <- f
  unless (s1 == ed) $ do
    solve s1 <$> f >>= B.putStrLn
    main
  where
    f = head <$> B.words <$> B.getLine
    ed = B.pack "."

solve :: ByteString -> ByteString -> ByteString
solve s1 s2 = f s1 s2 0
  where
    idnt = B.pack "IDENTICAL"
    cls = B.pack "CLOSE"
    dff = B.pack "DIFFERENT"
    f s1 s2 ct = 
      case (B.uncons s1, B.uncons s2) of
        (Nothing, Nothing) -> case ct of
                                0 -> idnt
                                1 -> cls
                                _ -> dff
        (Just (c1, s1'), Just (c2, s2')) -> if c1 /= c2 then dff else
          if c1 == '"' then let (x1, y1) = B.span (/= '"') s1'
                                (x2, y2) = B.span (/= '"') s2'
                            in if x1 == x2 then f (B.tail y1) (B.tail y2) ct
                               else f (B.tail y1) (B.tail y2) (ct + 1)
          else f s1' s2' ct
          
        _ -> dff
