{-# LANGUAGE ForeignFunctionInterface #-}
module CTypes 
  where
import           Foreign
import           Foreign.C.Types

#include "marchingcubes.h"

data CXYZ = CXYZ {
    __x :: CDouble
  , __y :: CDouble
  , __z :: CDouble
} deriving Show

instance Storable CXYZ where
    sizeOf    __ = #{size XYZ}
    alignment __ = #{alignment XYZ}
    peek ptr = do
      x' <- #{peek XYZ, x} ptr
      y' <- #{peek XYZ, y} ptr
      z' <- #{peek XYZ, z} ptr
      return CXYZ { __x = x'
                  , __y = y'
                  , __z = z' }
    poke ptr (CXYZ r1 r2 r3)
      = do
          #{poke XYZ, x} ptr r1
          #{poke XYZ, y} ptr r2
          #{poke XYZ, z} ptr r3

type XYZ = (Double, Double, Double)

cXYZtoXYZ :: CXYZ -> XYZ
cXYZtoXYZ (CXYZ x y z) = (realToFrac x, realToFrac y, realToFrac z)


data CTRIANGLE = CTRIANGLE {
  __p :: [CXYZ]
} deriving Show

type Triangle = (XYZ, XYZ, XYZ)

cTriangleToTriangle :: CTRIANGLE -> Triangle
cTriangleToTriangle (CTRIANGLE cxyzs) = (xyz0, xyz1, xyz2)
  where 
  xyzs = map cXYZtoXYZ cxyzs
  xyz0 = xyzs !! 0 
  xyz1 = xyzs !! 1
  xyz2 = xyzs !! 2 

instance Storable CTRIANGLE where
  sizeOf    __ = #{size TRIANGLE}
  alignment __ = #{alignment TRIANGLE}
  peek ptr = do
    p' <- peekArray 3 $ #{ptr TRIANGLE, p} ptr
    return CTRIANGLE { __p = p' }
  poke ptr (CTRIANGLE r1) = do
    pokeArray (#{ptr TRIANGLE, p} ptr) r1

foreign import ccall unsafe "testTriangle" c_testTriangle
  :: CDouble -> CDouble -> CDouble 
  -> IO (Ptr CTRIANGLE)


data CGRIDCELL = CGRIDCELL {
    ___p  :: [CXYZ]
  , __val :: [CDouble]
} deriving Show

instance Storable CGRIDCELL where
  sizeOf    __ = #{size GRIDCELL}
  alignment __ = #{alignment GRIDCELL}
  peek ptr = do
    p'   <- peekArray 8 $ #{ptr GRIDCELL, p} ptr
    val' <- peekArray 8 $ #{ptr GRIDCELL, val} ptr
    return CGRIDCELL { ___p  = p'
                     , __val = val' }
  poke ptr (CGRIDCELL r1 r2) = do
    pokeArray (#{ptr GRIDCELL, p} ptr) r1
    pokeArray (#{ptr GRIDCELL, val} ptr) r2

foreign import ccall unsafe "PolygoniseTri" c_PolygoniseTri
  :: Ptr CGRIDCELL -> CDouble -> Ptr CTRIANGLE
  -> CInt -> CInt -> CInt -> CInt 
  -> IO CInt

data GridCell = GridCell {
    _p   :: [XYZ]
  , _val :: [Double]
} deriving Show

cGridCellToGridCell :: CGRIDCELL -> GridCell
cGridCellToGridCell (CGRIDCELL cxyzs cvals) = 
  GridCell { _p = map cXYZtoXYZ cxyzs, _val = map realToFrac cvals}

gridCellToCGridCell :: GridCell -> CGRIDCELL
gridCellToCGridCell (GridCell xyzs vals) = 
  CGRIDCELL {  ___p  = map hXYZtoCXYZ xyzs
             , __val = map realToFrac vals}
  where 
    hXYZtoCXYZ (x,y,z) = CXYZ {  __x = realToFrac x
                               , __y = realToFrac y
                               , __z = realToFrac z }