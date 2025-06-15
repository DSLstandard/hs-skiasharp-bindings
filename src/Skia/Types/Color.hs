module Skia.Types.Color where

import Data.Coerce
import Foreign
import Foreign.C.Types
import Skia.Bindings
import Skia.Internal.Utils
import System.IO.Unsafe

data RGBA a = RGBA
    { red :: a
    , green :: a
    , blue :: a
    , alpha :: a
    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | 32-bit ARGB color value, unpremultiplied. Color components are always in a
known order. This is different from 'SKPMColor', which has its bytes in a
configuration dependent order, to match the format of kBGRA_8888_SkColorType
bitmaps. SkColor is the type used to specify colors in SkPaint and in gradients.

Color that is premultiplied has the same component values as color that is
unpremultiplied if alpha is 255, fully opaque, although may have the component
values in a different order.
-}
newtype SKColor = SKColor
    { unSKColor :: Sk_color -- Sk_color is a type alias to Word32. Use newtype to prevent accidental use.
    }
    deriving (Show, Eq, Ord)
    deriving newtype (Storable, Bits, Num)

{- | 32-bit ARGB color value, premultiplied. The byte order for this value is
configuration dependent, matching the format of 'SKColorType'BGRA8888' bitmaps.
This is different from SkColor, which is unpremultiplied, and is always in the
same byte order.
-}
newtype SKPMColor = SKPMColor
    { unSKPMColor :: Sk_pmcolor -- Sk_pmcolor is a type alias to Word32. Use newtype to prevent accidental use.
    }
    deriving (Show, Eq, Ord)
    deriving newtype (Storable, Bits, Num)

premultiply :: SKColor -> SKPMColor
premultiply (SKColor color) = SKPMColor $ unsafeDupablePerformIO do
    sk_color_premultiply color
{-# NOINLINE premultiply #-}

unpremultiply :: SKPMColor -> SKColor
unpremultiply (SKPMColor color) = SKColor $ unsafeDupablePerformIO do
    sk_color_unpremultiply color
{-# NOINLINE unpremultiply #-}
