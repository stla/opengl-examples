module Utils.ConvertPPM
  where
import           Control.Monad    (when)
import           Graphics.Image
import           System.Directory (removeFile)

convert :: FilePath -> FilePath -> Bool -> IO ()
convert input output remove = do
  ppm <- readImageRGBA VU input
  writeImage output ppm
  when remove $ removeFile input
