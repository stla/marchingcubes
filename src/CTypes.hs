{-# LINE 1 "CTypes.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module CTypes
  where
import           Foreign
import           Foreign.C.Types



data CXYZ = CXYZ {
    __x :: CDouble
  , __y :: CDouble
  , __z :: CDouble
} deriving Show

instance Storable CXYZ where
    sizeOf    __ = (24)
{-# LINE 17 "CTypes.hsc" #-}
    alignment __ = 8
{-# LINE 18 "CTypes.hsc" #-}
    peek ptr = do
      x' <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 20 "CTypes.hsc" #-}
      y' <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 21 "CTypes.hsc" #-}
      z' <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 22 "CTypes.hsc" #-}
      return CXYZ { __x = x'
                  , __y = y'
                  , __z = z' }
    poke ptr (CXYZ r1 r2 r3)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 28 "CTypes.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 29 "CTypes.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 30 "CTypes.hsc" #-}

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
  sizeOf    __ = (72)
{-# LINE 53 "CTypes.hsc" #-}
  alignment __ = 8
{-# LINE 54 "CTypes.hsc" #-}
  peek ptr = do
    p' <- peekArray 3 $ (\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr
{-# LINE 56 "CTypes.hsc" #-}
    return CTRIANGLE { __p = p' }
  poke ptr (CTRIANGLE r1) = do
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr) r1
{-# LINE 59 "CTypes.hsc" #-}

foreign import ccall unsafe "testTriangle" c_testTriangle
  :: CDouble -> CDouble -> CDouble
  -> IO (Ptr CTRIANGLE)


data CGRIDCELL = CGRIDCELL {
    ___p  :: [CXYZ]
  , __val :: [CDouble]
} deriving Show

instance Storable CGRIDCELL where
  sizeOf    __ = (256)
{-# LINE 72 "CTypes.hsc" #-}
  alignment __ = 8
{-# LINE 73 "CTypes.hsc" #-}
  peek ptr = do
    p'   <- peekArray 8 $ (\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr
{-# LINE 75 "CTypes.hsc" #-}
    val' <- peekArray 8 $ (\hsc_ptr -> hsc_ptr `plusPtr` 192) ptr
{-# LINE 76 "CTypes.hsc" #-}
    return CGRIDCELL { ___p  = p'
                     , __val = val' }
  poke ptr (CGRIDCELL r1 r2) = do
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 0) ptr) r1
{-# LINE 80 "CTypes.hsc" #-}
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 192) ptr) r2
{-# LINE 81 "CTypes.hsc" #-}

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
