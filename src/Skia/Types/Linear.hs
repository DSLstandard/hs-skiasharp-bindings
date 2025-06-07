module Skia.Types.Linear where

import Data.Coerce
import Foreign.C.Types
import Linear
import Skia.Bindings

toSKMatrix :: M33 Float -> Sk_matrix
toSKMatrix (coerce -> V3 (V3 scaleX skewX transX) (V3 skewY scaleY transY) (V3 persp0 persp1 persp2)) = Sk_matrix{..}

fromSKMatrix :: Sk_matrix -> M33 Float
fromSKMatrix Sk_matrix{..} = coerce $ V3 (V3 scaleX skewX transX) (V3 skewY scaleY transY) (V3 persp0 persp1 persp2)

toSKMatrix44 :: M44 Float -> Sk_matrix44
toSKMatrix44 (coerce -> V4 (V4 m00 m01 m02 m03) (V4 m10 m11 m12 m13) (V4 m20 m21 m22 m23) (V4 m30 m31 m32 m33)) = Sk_matrix44{..}

fromSKMatrix44 :: Sk_matrix44 -> M44 Float
fromSKMatrix44 Sk_matrix44{..} = coerce $ V4 (V4 m00 m01 m02 m03) (V4 m10 m11 m12 m13) (V4 m20 m21 m22 m23) (V4 m30 m31 m32 m33)

fromSKPoint :: Sk_point -> V2 Float
fromSKPoint Sk_point{..} = coerce (V2 x y)

toSKPoint :: V2 Float -> Sk_point
toSKPoint (coerce -> V2 x y) = Sk_point{..}

fromSKIPoint :: Sk_ipoint -> V2 Int
fromSKIPoint Sk_ipoint{..} = fmap fromIntegral (V2 x y)

toSKIPoint :: V2 Int -> Sk_ipoint
toSKIPoint (fmap fromIntegral -> V2 x y) = Sk_ipoint{..}

fromSKISize :: Sk_isize -> V2 Int
fromSKISize Sk_isize{..} = fmap fromIntegral (V2 w h)

toSKISize :: V2 Int -> Sk_isize
toSKISize (fmap fromIntegral -> V2 w h) = Sk_isize{..}

fromSKPoint3 :: Sk_point3 -> V3 Float
fromSKPoint3 Sk_point3{..} = coerce (V3 x y z)

toSKPoint3 :: V3 Float -> Sk_point3
toSKPoint3 (coerce -> V3 x y z) = Sk_point3{..}
