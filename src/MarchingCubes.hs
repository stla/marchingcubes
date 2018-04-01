module MarchingCubes
  where
import           Control.Monad.Extra   (concatMapM)
import           CTypes
-- import           Data.List.Index       (indexed)
import           Foreign.Marshal.Alloc (free, malloc, mallocBytes)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Storable      (poke, sizeOf)

-- algo tetrahedra
polygoniseTri :: GridCell
              -> Double
              -> Int -> Int -> Int -> Int
              -> IO [Triangle]
polygoniseTri gridcell iso i0 i1 i2 i3 = do
    let cgridcell = gridCellToCGridCell gridcell
    cgridcellPtr <- mallocBytes (sizeOf (undefined :: CGRIDCELL))
    poke cgridcellPtr cgridcell
    ctrianglesPtr <- malloc
    ntri <- c_PolygoniseTri cgridcellPtr (realToFrac iso) ctrianglesPtr
                            (fromIntegral i0) (fromIntegral i1)
                            (fromIntegral i2) (fromIntegral i3)
    ctriangles <- peekArray (fromIntegral ntri) ctrianglesPtr
    let triangles = map cTriangleToTriangle ctriangles
    free ctrianglesPtr
    return triangles

polygoniseTri' :: Double
               -> GridCell
               -> IO [Triangle]
polygoniseTri' iso gridcell = do
{-  triangles1 <- polygoniseTri gridcell iso 0 3 2 6
  triangles2 <- polygoniseTri gridcell iso 0 3 7 6
  triangles3 <- polygoniseTri gridcell iso 0 4 7 6
  triangles4 <- polygoniseTri gridcell iso 0 7 1 3
  triangles5 <- polygoniseTri gridcell iso 0 7 1 4
  triangles6 <- polygoniseTri gridcell iso 5 7 1 4 -}
  triangles1 <- polygoniseTri gridcell iso 0 5 1 3
  triangles2 <- polygoniseTri gridcell iso 0 5 7 3
  triangles3 <- polygoniseTri gridcell iso 0 2 7 3
  triangles4 <- polygoniseTri gridcell iso 0 7 4 5
  triangles5 <- polygoniseTri gridcell iso 0 7 4 2
  triangles6 <- polygoniseTri gridcell iso 6 7 4 2
  return $ triangles1 ++ triangles2 ++ triangles3 ++ triangles4 ++
           triangles5 ++ triangles6

-- algo marching cubes
polygonise :: Double
           -> GridCell
           -> IO [Triangle]
polygonise iso gridcell = do
    let cgridcell = gridCellToCGridCell gridcell
    cgridcellPtr <- mallocBytes (sizeOf (undefined :: CGRIDCELL))
    poke cgridcellPtr cgridcell
    ctrianglesPtr <- malloc
    ntri <- c_Polygonise cgridcellPtr (realToFrac iso) ctrianglesPtr
    ctriangles <- peekArray (fromIntegral ntri) ctrianglesPtr
    let triangles = map cTriangleToTriangle ctriangles
    free ctrianglesPtr
    return triangles

toGridCell :: [XYZ] -> [Double] -> GridCell
toGridCell xyzs vals = GridCell { _p = xyzs, _val = vals }

atest :: IO [Triangle]
atest = do
    let xyzs = [ (0, 0, 0)
               , (1, 0, 0)
               , (1, 1, 0)
               , (0, 1, 0)
               , (0, 0, 1)
               , (1, 0, 1)
               , (1, 1, 1)
               , (0, 1, 1) ]
        vals = [1, 2, 3, 4, 5, 6, 7, 8]
        gridcell = toGridCell xyzs vals
    polygoniseTri' 4 gridcell

atest' :: IO [Triangle]
atest' = do
    let xyzs = [ (0, 0, 0)
                , (1, 0, 0)
                , (1, 1, 0)
                , (0, 1, 0)
                , (0, 0, 1)
                , (1, 0, 1)
                , (1, 1, 1)
                , (0, 1, 1) ]
        vals = [1, 2, 3, 4, 5, 6, 7, 8]
        gridcell = toGridCell xyzs vals
    polygonise 4 gridcell

cube :: (Int,Int,Int) -> [XYZ]
cube (i,j,k) = [(dbl a, dbl b, dbl c) | a <- [i,i+1], b <- [j,j+1], c <- [k,k+1]]
  where
    dbl :: Int -> Double
    dbl = realToFrac

baseGrid :: Int -> [[XYZ]]
baseGrid n = map cube [(i, j, k) | i <- [0 .. n], j <- [0 .. n], k <- [0 .. n]]

scaleCube :: Int -> Double -> Double -> [XYZ] -> [XYZ]
scaleCube n a b = map scale
    where
    scale (x, y, z) = (s x, s y, s z)
      where
      s u = a + (b-a)*u / realToFrac (n+1)

voxelGrid :: Int -> Double -> Double -> [[XYZ]]
voxelGrid n a b = map (scaleCube n a b) (baseGrid n)

-- ~~ EXAMPLES ~~ --

-- SPHERE
fsphere :: XYZ -> Double
fsphere (x,y,z) = x*x + y*y + z*z

gridcells_sphere :: [GridCell]
gridcells_sphere = map (\vcube -> toGridCell vcube (map fsphere vcube))
                       (voxelGrid 5 (-1) 1)

triangles_sphere :: IO [Triangle]
triangles_sphere = concatMapM (polygoniseTri' 1) gridcells_sphere

-- GOURSAT
fGoursat :: XYZ -> Double
fGoursat (x,y,z) = x**4 + y**4 + z**4 - 0.27*(x**2+y**2+z**2)**2 - 0.5*(x**2+y**2+z**2)

gridcells_Goursat :: [GridCell]
gridcells_Goursat = map (\vcube -> toGridCell vcube (map fGoursat vcube))
                        (voxelGrid 20 (-2) 2)

triangles_Goursat :: IO [Triangle]
triangles_Goursat = concatMapM (polygoniseTri' 2) gridcells_Goursat

triangles_Goursat' :: IO [Triangle]
triangles_Goursat' = concatMapM (polygonise 2) gridcells_Goursat

-- HEART
fHeart :: XYZ -> Double
fHeart (x,y,z) = (2*x**2+y**2+z**2-1)**3 - x**2*z**3/10 - y**2*z**3

gridcells_Heart :: [GridCell]
gridcells_Heart = map (\vcube -> toGridCell vcube (map fHeart vcube))
                      (voxelGrid 50 (-4) 4)

triangles_Heart :: IO [Triangle]
triangles_Heart = concatMapM (polygoniseTri' 0) gridcells_Heart

triangles_Heart' :: IO [Triangle]
triangles_Heart' = concatMapM (polygonise 0) gridcells_Heart

-- ~~ MAIN FUNCTION ~~ --
marchingCubes :: (XYZ -> Double)   -- function
              -> Double            -- isolevel
              -> Double -> Double  -- bounds (common to x,y,z)
              -> Int               -- grid subdivisions
              -> IO [Triangle]
marchingCubes f level a b n = concatMapM (polygonise level) gridcells
  where
  gridcells = map (\vcube -> toGridCell vcube (map f vcube)) (voxelGrid n a b)
