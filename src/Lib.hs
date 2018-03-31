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
