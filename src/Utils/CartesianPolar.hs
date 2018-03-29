module Utils.CartesianPolar where
-- import           Data.List
-- import           Data.Tuple.Extra             (both)
-- import           Graphics.Rendering.OpenGL.GL (GLfloat, Vertex3 (..))

cartesianToPolar :: (Floating a, RealFloat a, RealFrac a) => (a, a, a) -> (a, a, a)
cartesianToPolar (x,y,z) =
  let r = sqrt(x*x+y*y+z*z) in
  (r, atan2 y x, acos z / r)

cartesianToPolar' :: (Floating a, RealFloat a, RealFrac a) => [a] -> (a, a, a)
cartesianToPolar' xyz =
  let x = (xyz !! 0) in
  let y = (xyz!! 1) in let z = (xyz !! 2) in
  let r = sqrt(x*x+y*y+z*z) in
  (r, atan2 y x, acos z / r)

polarToCartesian :: (Floating a, RealFrac a) => (a, a, a) -> (a, a, a)
polarToCartesian (r, theta, phi) =
  (r * cos theta * sin phi, r * sin theta * sin phi, r * cos phi)
