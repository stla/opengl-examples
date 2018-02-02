module Tutorial2.Points where
import           Graphics.Rendering.OpenGL

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
  where n' = fromIntegral n
