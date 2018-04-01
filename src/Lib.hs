module Lib
  where
import           CTypes
-- import           Foreign
import           Control.Monad.Extra   (concatMapM)
import           Data.List.Index       (indexed)
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

polygoniseTri' :: Double
               -> GridCell
               -> IO [Triangle]
polygoniseTri' iso gridcell = do
  triangles1 <- polygoniseTri gridcell iso 0 3 2 6
  triangles2 <- polygoniseTri gridcell iso 0 3 7 6
  triangles3 <- polygoniseTri gridcell iso 0 4 7 6
  triangles4 <- polygoniseTri gridcell iso 0 7 1 3
  triangles5 <- polygoniseTri gridcell iso 0 7 1 4
  triangles6 <- polygoniseTri gridcell iso 5 7 1 4
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
    polygoniseTri' 4 gridcell

{- cube1 :: [XYZ]
cube1 = [(toDbl i, toDbl j, toDbl k) | i <- [0,1], j <- [0,1], k <- [0,1]]
  where
    toDbl :: Int -> Double
    toDbl = realToFrac -}

cube :: (Int,Int,Int) -> [XYZ]
cube (i,j,k) = [(dbl a, dbl b, dbl c) | a <- [i,i+1], b <- [j,j+1], c <- [k,k+1]]
  where
    dbl :: Int -> Double
    dbl = realToFrac

{- shift_x,shift_y,shift_z :: Double -> XYZ -> XYZ
shift_x s (x,y,z) = (x+s,y,z)
shift_y s (x,y,z) = (x,y+s,z)
shift_z s (x,y,z) = (x,y,z+s) -}

fsphere :: XYZ -> Double
fsphere (x,y,z) = x*x + y*y + z*z

fGoursat :: XYZ -> Double
fGoursat (x,y,z) = x**4 + y**4 + z**4 - 0.27*(x**2+y**2+z**2)**2 - 0.5*(x**2+y**2+z**2)

fHeart :: XYZ -> Double
fHeart (x,y,z) = (2*x**2+y**2+z**2-1)**3-x**2*z**3/10-y**2*z**3

{- range_x,range_y,range_z :: [Double]
range_x = [2 * frac i n - 1| i <- [0 .. n]]
  where
    n = 4
    frac :: Int -> Int -> Double
    frac p q = realToFrac p / realToFrac q
range_y = range_x
range_z = range_x -}

-- voxelGrid :: [(Int, XYZ)]
-- voxelGrid = indexed [(x,y,z) | x <- range_x, y <- range_y, z <- range_z]

-- baseGrid :: [(Int, XYZ)]
-- baseGrid = indexed [(toDbl i, toDbl j, toDbl k) | i <- [0 .. n], j <- [0 .. n], k <- [0 .. n]]
--   where
--       n = 4
--       toDbl :: Int -> Double
--       toDbl = realToFrac

baseGrid :: [[XYZ]]
baseGrid = map cube [(i, j, k) | i <- [0 .. n], j <- [0 .. n], k <- [0 .. n]]
  where
    n = 9

scaleCube :: Double -> Double -> [XYZ] -> [XYZ]
scaleCube a b = map scale
  where
  scale (x, y, z) = (s x, s y, s z)
    where
    s u = (b-a)*u/10 + a

voxelGrid :: Double -> Double -> [[XYZ]]
voxelGrid a b = map (scaleCube a b) baseGrid

gridcells_sphere :: [GridCell]
gridcells_sphere = map (\vcube -> toGridCell vcube (map fsphere vcube))
                       (voxelGrid (-1) 1)

triangles_sphere :: IO [Triangle]
triangles_sphere = concatMapM (polygoniseTri' 1) gridcells_sphere

gridcells_Goursat :: [GridCell]
gridcells_Goursat = map (\vcube -> toGridCell vcube (map fGoursat vcube))
                        (voxelGrid (-2) 2)

triangles_Goursat :: IO [Triangle]
triangles_Goursat = concatMapM (polygoniseTri' 2) gridcells_Goursat

gridcells_Heart :: [GridCell]
gridcells_Heart = map (\vcube -> toGridCell vcube (map fHeart vcube))
                        (voxelGrid (-4) 4)

triangles_Heart :: IO [Triangle]
triangles_Heart = concatMapM (polygoniseTri' 0) gridcells_Heart
