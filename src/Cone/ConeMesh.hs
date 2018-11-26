module Cone.ConeMesh
  where
import qualified Data.Vector as V
import           Data.Vector     (Vector, fromList)

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
    d = if r < rr then h' * r / rr else h' * rr / r
    hdivhmd = h' / (h' - d)
    -- grid --------------------------------------------------------------------
    -- nstacks' = nstacks + 1;
    -- i_ = V.enumFromN 0 nstacks
    -- u_ = V.map (\i -> ((h'-d)/realToFrac nstacks) * i) i_
    -- j_ = V.enumFromN 0 (nslices-1)
    -- v_ = V.map (\j -> (2*i/realToFrac nslices) * j) j_
    -- grid = [(u_ ! i, v ! j) |... vecteur...]
    i_ = [0 .. nstacks]
    j_ = [0 .. nslices-1]
    u_ = [fromIntegral i * (h'-d)/fromIntegral nstacks | i <- i_]
    v_ = [fromIntegral j * 2 * pi / fromIntegral nslices | j <- j_]
    grid = [(u,v) | u <- u_, v <- v_]
    grid' = fromList grid
    gridij = [(i,j) | i <- tail i_, j <- j_]
    -- vertices ----------------------------------------------------------------
    vertices_normals =
      V.map (\(u,v) -> let g = h' - u in
                       let f = if r<rr then u * hdivhmd else u * hdivhmd - h' in
                       let cosv = rr/h' * cos v in
                       let sinv = rr/h' * sin v in
                       let t1 = (-cosv, -sinv, hdivhmd) in
                       let t2 = (-g * sinv, g*cosv, 0) in
                       ((g*cosv, g*sinv, f), nnormalize $ cross t1 t2)
            )
            grid'
    -- quads -----------------------------------------------------------------
    quads = map (\(i,j) -> let jp1 = if j<nslices-1 then j+1 else 0 in
                           (i*nslices+j, (i-1)*nslices+j, (i-1)*nslices+jp1, i*nslices+jp1))
                gridij
