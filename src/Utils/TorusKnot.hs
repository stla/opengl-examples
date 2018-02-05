module Utils.TorusKnot
  where
import           Graphics.Rendering.OpenGL.GL (Vertex3 (..), GLint)

torusKnot :: GLint -> GLint -> [Vertex3 Double]
torusKnot p q =
  map (toVertex3 . (\phi -> let r = cos (q'*phi) + 2 in
                            [ r * cos (p'*phi)
                            , r * sin (p'*phi)
                            , - sin (q'*phi)]))
      [intToDbl i * 2 * pi / 300 | i <- [0 .. 299]]
  where
    p' = realToFrac p
    q' = realToFrac q
    toVertex3 x = Vertex3 (x!!0) (x!!1) (x!!2)
    intToDbl :: Int -> Double
    intToDbl = realToFrac
