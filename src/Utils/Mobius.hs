module Utils.Mobius
  (mobiusStrip, mobiusCurve, mobiusStrip', mobiusCurve', mobiusStrip'')
  where
import           Data.Tuple.Extra             ((&&&))
import           Graphics.Rendering.OpenGL.GL (Normal3 (..), Vertex3 (..))
import           Linear                       (V3 (..), cross, signorm, (^-^))

type Rectangle = (V3 Double, V3 Double, V3 Double, V3 Double)
type Normal = V3 Double
type Vx3d = Vertex3 Double

intToDbl :: Int -> Double
intToDbl = realToFrac

mobiusStrip_ :: Double -> Double -> [(Rectangle, Normal)]
mobiusStrip_ hwidth radius =
  zip rectangles normals
  where
    n = 400
    u_ = [intToDbl i / intToDbl n * 4 * pi | i <- [0 .. n]] -- go to 4pi for the color issue
    c1 = map (\u -> V3 (cos u * (radius - hwidth * cos(u/2)))
                       (sin u * (radius - hwidth * cos(u/2)))
                       (-hwidth * sin(u/2))) u_
    c2 = map (\u -> V3 (cos u * (radius + hwidth * cos(u/2)))
                       (sin u * (radius + hwidth * cos(u/2)))
                       (hwidth * sin(u/2))) u_
    rectangles = map (\i -> (c1!!i, c2!!i, c2!!(i+1), c1!!(i+1))) [0 .. n-1]
    normals = map (\i -> normal (c1!!i) (c2!!i) (c2!!(i+1))) [0 .. n-1]
    normal v1 v2 v3 = signorm $ cross (v2^-^v1) (v3^-^v1)

mobiusStrip :: Double -> Double -> [((Vx3d, Vx3d, Vx3d, Vx3d), Normal3 Double)]
mobiusStrip hw r = map f (mobiusStrip_ hw r)
  where
    f ((v1, v2, v3, v4), n) =
      ((v3toVx3 v1, v3toVx3 v2, v3toVx3 v3, v3toVx3 v4), v3toN n)
    v3toVx3 :: V3 a -> Vertex3 a
    v3toVx3 (V3 x y z) = Vertex3 x y z
    v3toN :: V3 a -> Normal3 a
    v3toN (V3 x y z) = Normal3 x y z

mobiusCurve :: Double -> Double -> Int -> [Vx3d]
mobiusCurve hwidth radius n =
  c1
  where
    u_ = [intToDbl i / intToDbl n * 4 * pi | i <- [0 .. n]]
    c1 = map (\u -> Vertex3 (cos u * (radius - hwidth * cos(u/2)))
                            (sin u * (radius - hwidth * cos(u/2)))
                            (-hwidth * sin(u/2))) u_
    -- c2 = map (\u -> Vertex3 (cos u * (radius + hwidth * cos(u/2)))
    --                         (sin u * (radius + hwidth * cos(u/2)))
    --                         (hwidth * sin(u/2))) u_

-- | https://blogs.mathworks.com/cleve/2016/04/25/further-twists-of-the-moebius-strip/
mobiusStrip_' :: Int -> Double -> Double -> Double -> [(Rectangle, Normal)]
mobiusStrip_' n c w k =
  zip rectangles normals
  where
    t_ = [intToDbl i / intToDbl n | i <- [-2*n .. 2*n]]
    r1_ = map (\t -> (1-w) - w*sin(k*pi*t/2)) t_
    r2_ = map (\t -> (1-w) + w*sin(k*pi*t/2)) t_
    c1 = zipWith (\r t -> V3 (r * sin(c*pi*t) / c)
                             (r * (1-(1-cos(c*pi*t)) / c))
                             (-w * cos(k*pi*t/2))) r1_ t_
    c2 = zipWith (\r t -> V3 (r * sin(c*pi*t) / c)
                             (r * (1-(1-cos(c*pi*t)) / c))
                             (w * cos(k*pi*t/2))) r2_ t_
    rectangles = map (\i -> (c1!!i, c2!!i, c2!!(i+1), c1!!(i+1))) [0 .. 4*n-1]
    normals = map (\i -> normal (c1!!i) (c2!!i) (c2!!(i+1))) [0 .. 4*n-1]
    normal v1 v2 v3 = signorm $ cross (v2^-^v1) (v3^-^v1)

mobiusStrip' :: Int -> Double -> Double -> Double
             -> [((Vx3d, Vx3d, Vx3d, Vx3d), Normal3 Double)]
mobiusStrip' n c w k = map f (mobiusStrip_' n c w k)
  where
    f ((v1, v2, v3, v4), nrml) =
      ((v3toVx3 v1, v3toVx3 v2, v3toVx3 v3, v3toVx3 v4), v3toN nrml)
    v3toVx3 :: V3 a -> Vertex3 a
    v3toVx3 (V3 x y z) = Vertex3 x y z
    v3toN :: V3 a -> Normal3 a
    v3toN (V3 x y z) = Normal3 x y z

mobiusCurve' :: Int -> Double -> Double -> Double -> ([Vx3d], [Vx3d])
mobiusCurve' n c w k =
  (c1, c2)
  where
    t_ = [intToDbl i / intToDbl n | i <- [0 .. 2*n]]
    r1_ = map (\t -> (1-w) - w*sin(k*pi*t/2)) t_
    r2_ = map (\t -> (1-w) + w*sin(k*pi*t/2)) t_
    c1 = zipWith (\r t -> Vertex3 (r * sin(c*pi*t) / c)
                                  (r * (1-(1-cos(c*pi*t)) / c))
                                  (-w * cos(k*pi*t/2))) r1_ t_
    c2 = zipWith (\r t -> Vertex3 (r * sin(c*pi*t) / c)
                                  (r * (1-(1-cos(c*pi*t)) / c))
                                  (w * cos(k*pi*t/2))) r2_ t_

mobiusStrip_'' :: Int -> Int -> Double -> Double -> Double
               -> [(Rectangle, Normal)]
mobiusStrip_'' m n c w k = concat nrectgles
  where
  s_ = [intToDbl i / intToDbl m | i <- [-m .. m]]
  t_ = [intToDbl i / intToDbl n | i <- [-2*n .. 2*n]]
  curves = map (\s -> map (f s) t_) s_
    where
    f s t = let r = (1-w) + s*w*sin(k*pi*t/2) in
            V3 (r * sin(c*pi*t) / c)
               (r * (1-(1-cos(c*pi*t)) / c))
               (s * w * cos(k*pi*t/2))
  nrectgles = zipWith rn (init curves) (tail curves)
    where
    rn c1 c2 = map (rctgle &&& nrml) [0 .. 4*n-1]
      where
      rctgle i = (c1!!i, c2!!i, c2!!(i+1), c1!!(i+1))
      nrml i = normal (c1!!i) (c2!!i) (c2!!(i+1))
        where
        normal v1 v2 v3 = signorm $ cross (v2^-^v1) (v3^-^v1)

mobiusStrip'' :: Int -> Int -> Double -> Double -> Double
              -> [((Vx3d, Vx3d, Vx3d, Vx3d), Normal3 Double)]
mobiusStrip'' m n c w k = map f (mobiusStrip_'' m n c w k)
  where
    f ((v1, v2, v3, v4), nrml) =
      ((v3toVx3 v1, v3toVx3 v2, v3toVx3 v3, v3toVx3 v4), v3toN nrml)
    v3toVx3 :: V3 a -> Vertex3 a
    v3toVx3 (V3 x y z) = Vertex3 x y z
    v3toN :: V3 a -> Normal3 a
    v3toN (V3 x y z) = Normal3 x y z
