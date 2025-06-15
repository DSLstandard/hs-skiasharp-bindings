module Skia.Types.Internal.Utils.Color where

import Data.Coerce
import Foreign
import Foreign.C.Types
import Skia.Bindings
import Skia.Internal.Utils
import Skia.Types.Color
import System.IO.Unsafe

toSKColor :: RGBA Word8 -> SKColor
toSKColor RGBA{red, green, blue, alpha} =
    SKColor $ convert4Word8ToWord32 (alpha, red, green, blue)

fromSKColor :: SKColor -> RGBA Word8
fromSKColor (SKColor color) = do
    let (alpha, red, green, blue) = convertWord32To4Word8 color
    RGBA{red, green, blue, alpha}

toSKColor4f :: RGBA Float -> Sk_color4f
toSKColor4f (coerce -> RGBA{..}) = Sk_color4f{fR = red, fG = green, fB = blue, fA = alpha}

fromSKColor4f :: Sk_color4f -> RGBA Float
fromSKColor4f Sk_color4f{fR = red, fG = green, fB = blue, fA = alpha} = coerce RGBA{..}
