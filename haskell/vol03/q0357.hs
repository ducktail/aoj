import Control.Applicative ((<$>))

main :: IO ()
main = flip div 2 <$> sum <$> map read <$> words <$> getLine >>= print
