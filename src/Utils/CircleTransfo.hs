module Utils.CircleTransfo
  where
import           Data.Foldable                           (toList)
import           Graphics.Rendering.OpenGL.GL            (GLdouble, GLfloat)
import           Graphics.Rendering.OpenGL.GL.CoordTrans
import           Linear

plane3pts :: Num a => V3 a -> V3 a -> V3 a -> (V3 a, a)
plane3pts p1 p2 p3 = (V3 xcoef ycoef zcoef, offset)
  where
    V3 p1x p1y p1z = p1
    V3 p2x p2y p2z = p2
    V3 p3x p3y p3z = p3
    xcoef = (p1y-p2y)*(p2z-p3z)-(p1z-p2z)*(p2y-p3y)
    ycoef = (p1z-p2z)*(p2x-p3x)-(p1x-p2x)*(p2z-p3z)
    zcoef = (p1x-p2x)*(p2y-p3y)-(p1y-p2y)*(p2x-p3x)
    offset = p1x*xcoef + p1y*ycoef + p1z*zcoef

plane1ptnormal :: Num a => V3 a -> V3 a -> (V3 a, a)
plane1ptnormal p normal = (normal, p `dot` normal)

circleCenterRadius :: (Num a, Fractional a, Floating a) => V3 a -> V3 a -> V3 a
                   -> ((V3 a, a), V3 a)
circleCenterRadius p1 p2 p3 = ((center, radius), coefs1)
  where
    p12 = (p1 ^+^ p2) ^/ 2
    p23 = (p2 ^+^ p3) ^/ 2
    v12 = p2 ^-^ p1
    v23 = p3 ^-^ p2
    (coefs1, offset1) = plane3pts p1 p2 p3
    (coefs2, offset2) = plane1ptnormal p12 v12
    (coefs3, offset3) = plane1ptnormal p23 v23
    a = V3 coefs1 coefs2 coefs3
    b = V3 offset1 offset2 offset3
    center = inv33 a !* b
    op1 = p1 ^-^ center
    radius = norm op1

transformationMatrix :: (Real a, Floating a) => V3 a -> V3 a -> V3 a
                     -> ([a], GLdouble)
transformationMatrix p1 p2 p3 =
  (concatMap toList (toList (mkTransformationMat m center)), radius')
  where
    ((center, radius), plane) = circleCenterRadius p1 p2 p3
    radius' = realToFrac radius
    --(plane,_) = plane3pts p1 p2 p3 -- c'est con tu le fais deux fois
    V3 a b c = plane
    measure = norm plane
    a' = a / measure
    b' = b / measure
    c' = c / measure
    n = V3 a' b' c'
--    s = sqrt (a'*a'+c'*c')
--    a'' = a'/s
--    c'' = c'/s
--    u = V3 (-c'') 0 a''

    s = sqrt (b'*b'+c'*c')
    b'' = b'/s
    c'' = c'/s
    u = V3 0 (-c'') b''

    v = cross n u
    m = V3 u n v


