module Lib
  where
import           CTypes
-- import           Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, malloc, mallocBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Storable      (peek, poke, sizeOf)

someFunc :: IO CTRIANGLE
someFunc = do
    ptr <- c_testTriangle 1 2 3
    peek ptr

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
                            (fromIntegral i0) (fromIntegral i1) (fromIntegral i2) (fromIntegral i3)
    ctriangles <- peekArray (fromIntegral ntri) ctrianglesPtr
    let triangles = map cTriangleToTriangle ctriangles
    free ctrianglesPtr
    return triangles

-- :: Ptr CGRIDCELL :: grille avec les valeurs
-- -> CDouble : isolevel
-- -> Ptr CTRIANGLE : "vide", c'est une sortie
-- -> CInt -> CInt -> CInt -> CInt : les 4 indices, voir le fichier C
-- -> IO CInt : le nombre de triangles calculés

-- GridCell = { _p :: 8 XYZ, _val: 8 double }
-- XYZ = (Double, Double, Double)

toGridCell :: [XYZ] -> [Double] -> GridCell
toGridCell xyzs vals = GridCell { _p = xyzs, _val = vals }

polygoniseTri' :: GridCell
               -> Double
               -> IO [Triangle]
polygoniseTri' gridcell iso = do
  triangles1 <- polygoniseTri gridcell iso 0 2 3 7
  triangles2 <- polygoniseTri gridcell iso 0 2 6 7
  triangles3 <- polygoniseTri gridcell iso 0 4 6 7
  triangles4 <- polygoniseTri gridcell iso 0 6 1 2
  triangles5 <- polygoniseTri gridcell iso 0 6 1 4
  triangles6 <- polygoniseTri gridcell iso 5 6 1 4
  return $ triangles1 ++ triangles2 ++ triangles3 ++ triangles4 ++ triangles5 ++ triangles6

atest :: IO [Triangle]
atest = do
    let f x = x*x
        xyzs = [ (0, 0, 0)
               , (1, 0, 0)
               , (1, 1, 0)
               , (0, 1, 0)
               , (0, 0, 1)
               , (1, 0, 1)
               , (1, 1, 1)
               , (0, 1, 1) ]
        vals = map f [1, 2, 3, 4, 5, 6, 7, 8]
        gridcell = toGridCell xyzs vals
    polygoniseTri' gridcell 4

cube1 :: [XYZ]
cube1 = [(toDbl i, toDbl j, toDbl k) | i <- [0,1], j <- [0,1], k <- [0,1]]
    where
        toDbl :: Int -> Double
        toDbl = realToFrac

shift_x,shift_y,shift_z :: Double -> XYZ -> XYZ
shift_x s (x,y,z) = (x+s,y,z)
shift_y s (x,y,z) = (x,y+s,z)
shift_z s (x,y,z) = (x,y,z+s)
