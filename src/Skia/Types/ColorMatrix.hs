module Skia.Types.ColorMatrix (
    ColorMatrix,
    unsafeWith,
    toVector,
    fromVector,
    fromColumns,
    toColumns,
    toM45,
    fromM45,
    identity,
)
where

-- The namespacing here is very confusing, so beware.

import Data.Foldable qualified
import Data.Vector qualified
import Data.Vector.Storable qualified
import Foreign
import Linear (V4 (..))
import Linear qualified
import Linear.V (V)
import Linear.V qualified

{- | A color matrix - a 4x5 matrix.

The 5th column is used for translation.

'ColorMatrix' is implemented as a 20-element vector. The elements are arranged
in row major order. You can only create a 'ColorMatrix' through the construction
functions in this module.

This structure is a pure Haskell implementation of @SkColorMatrix@ in Google's
Skia.
-}
newtype ColorMatrix a = ColorMatrix
    { unColorMatrix :: Data.Vector.Vector a
    }
    deriving (Eq, Ord)

instance (Show a) => Show (ColorMatrix a) where
    show matrix = "fromColumns " <> show (toColumns matrix)

{- | Pass a pointer to the matrix's data to the IO action. The data may not be
modified through the 'Ptr'.

This operation is O(1). There is no data copying.
-}
unsafeWith :: (Storable a) => ColorMatrix a -> (Ptr a -> IO r) -> IO r
unsafeWith (ColorMatrix array) = Data.Vector.Storable.unsafeWith (Data.Vector.convert array)

{- | O(1). Returns the underlying vector of the color matrix. There is no data
copying.
-}
toVector :: ColorMatrix a -> Data.Vector.Vector a
toVector = unColorMatrix

{- | O(1). Converts a vector to a color matrix. There is no data copying.

The input vector must have 20 elements, or else this function throws an
'error'.
-}
fromVector :: Data.Vector.Vector a -> ColorMatrix a
fromVector array =
    if len == 20
        then ColorMatrix array
        else error $ "The input array should have exactly 20 elements, but got " <> show len <> " element(s)."
  where
    len = Data.Vector.length array

{- | Given five 'V4' columns, r, g, b, a, and T; this function constructs the color matrix as such:

@
/ r0 g0 b0 a0 T0 \
| r1 g1 b1 a1 T1 |
| r2 g2 b2 a2 T2 |
\ r3 g3 b3 a3 T3 /
@
-}
fromColumns :: (V4 a, V4 a, V4 a, V4 a, V4 a) -> ColorMatrix a
fromColumns (c1, c2, c3, c4, c5) =
    ColorMatrix $ Data.Vector.fromListN 20 $ concat $ Linear.transpose [c1, c2, c3, c4, c5]

-- | The inverse of 'fromColumns'.
toColumns :: forall a. ColorMatrix a -> (V4 a, V4 a, V4 a, V4 a, V4 a)
toColumns (ColorMatrix array) = do
    (getColumn 0, getColumn 1, getColumn 2, getColumn 3, getColumn 4)
  where
    getColumn :: Int -> V4 a
    getColumn columnI =
        Linear.V4
            (Data.Vector.unsafeIndex array columnI)
            (Data.Vector.unsafeIndex array (columnI + 5))
            (Data.Vector.unsafeIndex array (columnI + 10))
            (Data.Vector.unsafeIndex array (columnI + 15))
    {-# INLINE getColumn #-}

toM45 :: forall a. ColorMatrix a -> V4 (V 5 a)
toM45 (ColorMatrix array) = V4 (getRow 0) (getRow 1) (getRow 2) (getRow 3)
  where
    getRow :: Int -> V 5 a
    getRow rowI = Linear.V.V $ Data.Vector.slice (rowI * 5) 5 array
    {-# INLINE getRow #-}

fromM45 :: V4 (V 5 a) -> ColorMatrix a
fromM45 m45 = ColorMatrix $ Data.Vector.convert $ Data.Vector.concat $ fmap Linear.V.toVector $ Data.Foldable.toList $ m45

-- | Returns the identity matrix.
identity :: (Storable a, Num a) => ColorMatrix a
identity =
    fromColumns
        ( V4 1 0 0 0
        , V4 0 1 0 0
        , V4 0 0 1 0
        , V4 0 0 0 1
        , V4 0 0 0 0
        )
