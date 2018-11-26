module Cone.ConeMesh
  where
import qualified Data.Vector as V
import           Data.Vector      (Vector, fromList)
import           Data.Foldable    (toList)
import           Linear hiding    (cross)
import qualified Linear as L
import           Control.Lens     ((^.))

cross :: Floating a => (a,a,a) -> (a,a,a) -> (a,a,a)
cross (v1,v2,v3) (w1,w2,w3) =
  (
  v2*w3 - v3*w2,
  v3*w1 - v1*w3,
  v1*w2 - v2*w1
  )

nnormalize :: Floating a => (a,a,a) -> (a,a,a)
nnormalize (x,y,z) = (-x/n,-y/n,-z/n)
  where
    n = sqrt(x*x+y*y+z*z)

cmesh0 :: (Ord a, Floating a) => a -> a -> a -> Int -> Int
       -> (Vector ((a,a,a),(a,a,a)), [(Int,Int,Int,Int)])
cmesh0 h rr r nstacks nslices = (vertices_normals, quads)
  where
    h' = if r < rr then h else -h
    ratio = if r < rr then r / rr else rr / r
    k = (ratio - 1) / h'
    i_ = [0 .. nstacks]
    j_ = [0 .. nslices-1]
    u_ = [fromIntegral i * h'/fromIntegral nstacks | i <- i_]
    v_ = [fromIntegral j * 2 * pi / fromIntegral nslices | j <- j_]
    grid = [(u,v) | u <- u_, v <- v_]
    grid' = fromList grid
    gridij = [(i,j) | i <- tail i_, j <- j_]
    -- higher radius
    rbig = if r<rr then rr else r
    -- vertices ----------------------------------------------------------------
    vertices_normals =
      V.map (\(u,v) -> let g = 1 + k*u in
                       let cosv = rbig * cos v in
                       let sinv = rbig * sin v in
                       let t1 = (k*cosv, k*sinv, 1) in
                       let t2 = (-g*sinv, g*cosv, 0) in
                       ((g*cosv, g*sinv, u), nnormalize $ cross t1 t2)
            )
            grid'
    -- quads -----------------------------------------------------------------
    quads = map (\(i,j) -> let jp1 = if j<nslices-1 then j+1 else 0 in
                           (i*nslices+j, (i-1)*nslices+j, (i-1)*nslices+jp1, i*nslices+jp1))
                gridij

transfoMatrixCone :: (Real a, Floating a) => V3 a -> V3 a -> ([a], a)
transfoMatrixCone cr1 cr2 =
  (concatMap toList (toList (mkTransformationMat m trans)), height)
  where
    normal' = cr2 ^-^ cr1
    height = norm normal'
    trans = cr1 -- ^-^ cr2
    normal = normal' ^/ height
    nx = normal ^. _x
    ny = normal ^. _y
    s = sqrt(nx*nx + ny*ny)
    u = V3 (ny/s) (-nx/s) 0
    v = L.cross normal u
    coefCase0 = if normal ^. _z >0 then 1 else -1
    m = if s == 0
          then V3 (V3 1 0 0) (V3 0 coefCase0 0) (V3 0 0 coefCase0)
          else transpose $ V3 u v normal

-- returns the mesh and the transformation matrix
coneMesh' :: (Real a, Floating a) => V3 a -> V3 a -> a -> a -> Int -> Int
         -> ((Vector ((a,a,a),(a,a,a)), [(Int,Int,Int,Int)]), [a])
coneMesh' cr1 cr2 r1 r2 nstacks nslices = (conemesh0, matrix)
  where
    (matrix, h) = transfoMatrixCone cr1 cr2
    conemesh0 = cmesh0 h r1 r2 nstacks nslices

coneMesh :: (Real a, Floating a) => V3 a -> V3 a -> a -> a -> Int -> Int
         -> ((Vector ((a,a,a),(a,a,a)), [(Int,Int,Int,Int)]), [a])
coneMesh cr1 cr2 r1 r2 nstacks nslices =
  if r2<r1
    then coneMesh' cr1 cr2 r1 r2 nstacks nslices
    else coneMesh' cr2 cr1 r2 r1 nstacks nslices
